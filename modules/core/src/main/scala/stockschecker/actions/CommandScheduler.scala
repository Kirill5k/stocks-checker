package stockschecker.actions

import cats.effect.Temporal
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import fs2.Stream
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.domain.{Command, Job, JobCommand}
import stockschecker.repositories.{CommandRepository, JobRepository}

import java.time.Instant
import scala.concurrent.duration.*

trait CommandScheduler[F[_]]:
  def tick: F[Unit]
  def run: Stream[F, Unit]

/** Scheduling and recovery never share the execution worker pool. */
final private class LiveCommandScheduler[F[_]: {Temporal, Clock, Logger}](
    commands: CommandRepository[F],
    jobs: JobRepository[F]
) extends CommandScheduler[F] {
  private def enqueue(command: Command, now: Instant): F[Unit] =
    command.activeRunId.traverse_ { id =>
      val acceptedAt = command.lastExecutedAt.getOrElse(now)
      val job        = Job(id, command.action, acceptedAt, acceptedAt, Some(JobCommand(command.id, command.version, id)))
      jobs.enqueue(job, ActionPriority.forAction(command.action))
    }

  private def recoverRun(command: Command, now: Instant): F[Option[Command]] =
    command.activeRunId.fold(Option(command).pure[F]) { id =>
      jobs.find(id).flatMap {
        case Some(_)                                         => Option(command).pure[F]
        case None if command.isActive && command.ownsRun(id) =>
          enqueue(command, now).as(Some(command))
        // An edit may replace the action after reservation but before enqueue. Never attach it to the old run ID.
        case None => commands.completeRun(command.id, id)
      }
    }

  private def completeRun(command: Command): F[Option[Command]] =
    command.activeRunId.fold(Option(command).pure[F]) { id =>
      jobs.find(id).flatMap {
        case Some(job) if job.isTerminal => commands.completeRun(command.id, id)
        case _                           => Option(command).pure[F]
      }
    }

  private def scheduleNext(command: Command, now: Instant): F[Option[Command]] =
    commands.initializeNextRun(command, now)

  private def reserveAndEnqueue(command: Command, now: Instant): F[Unit] =
    commands.reserve(command, now).flatMap(_.traverse_(enqueue(_, now)))

  override def tick: F[Unit] =
    for
      now     <- summon[Clock[F]].now
      pending <- commands.pending(now)
      _       <- pending.traverse_ { command =>
        (for
          recovered <- recoverRun(command, now)
          completed <- recovered.traverse(completeRun).map(_.flatten)
          ready     <- completed.traverse(scheduleNext(_, now)).map(_.flatten)
          _         <- ready.traverse_(reserveAndEnqueue(_, now))
        yield ()).handleErrorWith(error => summon[Logger[F]].error(error)(s"Command ${command.id.value} scheduling failed"))
      }
    yield ()

  override def run: Stream[F, Unit] =
    Stream
      .repeatEval(tick.handleErrorWith(error => summon[Logger[F]].error(error)("Command scheduler tick failed")))
      .metered(1.second)
}

object CommandScheduler:
  def make[F[_]: {Temporal, Clock, Logger}](commands: CommandRepository[F], jobs: JobRepository[F]): F[CommandScheduler[F]] =
    Temporal[F].pure(new LiveCommandScheduler(commands, jobs))
