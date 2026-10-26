package stockschecker.services

import cats.effect.kernel.Temporal
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{Command, CommandId, CreateCommand, JobCommand, Schedule, UpdateCommand}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{CommandRepository, JobRepository}

import scala.util.Try

trait CommandService[F[_]]:
  def create(cc: CreateCommand): F[Command]
  def executeManually(cid: CommandId): F[Unit]
  def getAll: F[List[Command]]
  def activate(cid: CommandId, isActive: Boolean): F[Unit]
  def update(uc: UpdateCommand): F[Command]
  def isCurrent(command: JobCommand): F[Boolean]

final private class LiveCommandService[F[_]](
    private val actionDispatcher: ActionDispatcher[F],
    private val repo: CommandRepository[F],
    private val jobs: JobRepository[F]
)(using
    F: Temporal[F],
    C: Clock[F]
) extends CommandService[F] {

  private def validateAction(action: Action): F[Unit] = action match
    case Action.LegacyRetired     => F.raiseError(AppError.FailedValidation("This scheduling or audit action has been retired"))
    case Action.Sequence(actions) => actions.traverse_(validateAction)
    case _                        => F.unit

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

  override def create(cc: CreateCommand): F[Command] =
    for
      _           <- validateAction(cc.action) >> validate(cc.schedule, cc.maxExecutions)
      now         <- C.now
      cmd         <- repo.create(cc)
      initialized <- repo.initializeNextRun(cmd, now)
    yield initialized.getOrElse(cmd)

  override def executeManually(cid: CommandId): F[Unit] =
    repo.find(cid).flatMap(cmd => validateAction(cmd.action) >> actionDispatcher.dispatch(cmd.action))

  override def getAll: F[List[Command]] = repo.all

  private def refresh(command: Command): F[Unit] =
    jobs.cancelObsolete(command.id, command.revision) >>
      C.now.flatMap(repo.initializeNextRun(command, _)).void

  override def activate(cid: CommandId, isActive: Boolean): F[Unit] =
    repo.setActive(cid, isActive).flatMap(refresh)

  override def update(uc: UpdateCommand): F[Command] =
    validateAction(uc.action) >> validate(uc.schedule, uc.maxExecutions) >> repo.update(uc).flatTap(refresh)

  override def isCurrent(command: JobCommand): F[Boolean] =
    repo.find(command.commandId).map(c => c.isActive && c.revision == command.version)
}

object CommandService:
  def make[F[_]: {Temporal, Clock}](repo: CommandRepository[F], ad: ActionDispatcher[F], jobs: JobRepository[F]): F[CommandService[F]] =
    Temporal[F].pure(LiveCommandService[F](ad, repo, jobs))
