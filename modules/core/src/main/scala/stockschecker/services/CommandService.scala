package stockschecker.services

import cats.effect.kernel.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import cats.syntax.either.*
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import stockschecker.actions.{Action, CommandScheduler}
import stockschecker.actions.ActionDispatcher
import stockschecker.domain.{Command, CommandId, CreateCommand, JobCommand, Schedule, UpdateCommand}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.CommandRepository

import scala.util.Try

trait CommandService[F[_]]:
  def rescheduleAll: F[Unit]
  def create(cc: CreateCommand): F[Command]
  def execute(cid: CommandId): F[Unit]
  def executeManually(cid: CommandId): F[Unit]
  def getAll: F[List[Command]]
  def activate(cid: CommandId, isActive: Boolean): F[Unit]
  def update(uc: UpdateCommand): F[Command]
  def isCurrent(command: JobCommand): F[Boolean]

final private class LiveCommandService[F[_]](
    private val actionDispatcher: ActionDispatcher[F],
    private val repo: CommandRepository[F]
)(using
    F: Temporal[F],
    C: Clock[F]
) extends CommandService[F] {

  private val jobs = Option(actionDispatcher.jobRepository).flatten

  private def validate(schedule: Schedule, maxExecutions: Option[Int]): F[Unit] =
    F.raiseWhen(maxExecutions.exists(_ <= 0) || (schedule match {
      case Schedule.Periodic(period) => period.toMillis < 1000L
      case _                         => false
    }))(AppError.FailedValidation("Schedule period must be at least one second and maxExecutions must be positive")) >>
      C.now.flatMap(now =>
        F.fromEither(
          Try(schedule.nextExecutionTime(now)).toEither.leftMap(_ => AppError.FailedValidation("Schedule has no valid next execution"))
        ).void
      )

  override def rescheduleAll: F[Unit] =
    jobs.fold(C.now.flatMap { now =>
      repo.streamActive
        .evalTap { cmd =>
          actionDispatcher.dispatch(Action.Schedule(cmd.id, cmd.durationUntilNextExecution(now)))
        }
        .compile
        .drain
    })(repository => CommandScheduler.tick(repo, repository))

  override def create(cc: CreateCommand): F[Command] =
    for
      _   <- validate(cc.schedule, cc.maxExecutions)
      now <- C.now
      cmd <- repo.create(cc)
      _   <- jobs.fold(actionDispatcher.dispatch(Action.Schedule(cmd.id, cmd.durationUntilNextExecution(now))))(_ =>
        repo.initializeNextRun(cmd, now)
      )
    yield cmd

  override def execute(cid: CommandId): F[Unit] =
    jobs.fold(for
      now <- C.now
      cmd <- repo.find(cid)
      _   <- F.whenA(cmd.canBeExecuted) {
        actionDispatcher.dispatch(cmd.action) >>
          repo.update(cmd.incExecutionCount(now))
      }
      nextExecTime = cmd.schedule.nextExecutionTime(now)
      _ <- actionDispatcher.dispatch(Action.Schedule(cid, now.durationBetween(nextExecTime)))
    yield ()) { repository =>
      for {
        now        <- C.now
        command    <- repo.find(cid)
        occurrence <- repo.reserve(command, now)
        _          <- occurrence.traverse_(o => repository.enqueue(o.job) >> repo.clearOutbox(cid, o.job.id))
      } yield ()
    }

  override def executeManually(cid: CommandId): F[Unit] =
    repo.find(cid).flatMap(cmd => actionDispatcher.dispatch(cmd.action))

  override def getAll: F[List[Command]] =
    repo.all

  override def activate(cid: CommandId, isActive: Boolean): F[Unit] =
    repo.setActive(cid, isActive) >> jobs.traverse_ { repository =>
      for {
        command <- repo.find(cid)
        now     <- C.now
        _       <- repository.cancelObsolete(cid, command.revision)
        _       <- repo.initializeNextRun(command, now)
      } yield ()
    }

  override def update(uc: UpdateCommand): F[Command] =
    validate(uc.schedule, uc.maxExecutions) >> repo.update(uc).flatTap { command =>
      jobs.traverse_(repository =>
        repository.cancelObsolete(command.id, command.revision) >> C.now.flatMap(repo.initializeNextRun(command, _))
      )
    }

  override def isCurrent(command: JobCommand): F[Boolean] =
    repo.find(command.commandId).map(c => c.isActive && c.revision == command.version)
}

object CommandService:
  def make[F[_]: {Temporal, Clock}](repo: CommandRepository[F], ad: ActionDispatcher[F]): F[CommandService[F]] =
    Temporal[F].pure(LiveCommandService[F](ad, repo))
