package stockschecker.actions

import cats.effect.Temporal
import cats.syntax.all.*
import fs2.Stream
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.repositories.{CommandRepository, JobRepository}

import scala.concurrent.duration.*

/** Scheduling and outbox recovery never share the execution worker pool. */
object CommandScheduler {
  def tick[F[_]: {Temporal, Clock}](commands: CommandRepository[F], jobs: JobRepository[F]): F[Unit] =
    for {
      now <- summon[Clock[F]].now
      all <- commands.all
      results <- all.traverse { command =>
        (for {
          _ <- command.outbox.traverse_ { occurrence =>
            jobs.enqueue(occurrence.job) >> commands.clearOutbox(command.id, occurrence.job.id)
          }
          _ <- jobs.cancelObsolete(command.id, command.revision)
          _ <- command.activeRunId.traverse_ { id =>
            jobs.find(id).flatMap(job => Temporal[F].whenA(job.exists(_.isTerminal))(commands.completeRun(command.id, id)))
          }
          fresh <- commands.find(command.id)
          _ <- commands.initializeNextRun(fresh, now)
          ready <- commands.find(command.id)
          occurrence <- commands.reserve(ready, now)
          _ <- occurrence.traverse_(o => jobs.enqueue(o.job) >> commands.clearOutbox(command.id, o.job.id))
        } yield ()).attempt
      }
      _ <- results.collectFirst { case Left(error) => error }.fold(Temporal[F].unit)(Temporal[F].raiseError[Unit])
    } yield ()

  def run[F[_]: {Temporal, Clock, Logger}](commands: CommandRepository[F], jobs: JobRepository[F]): Stream[F, Unit] =
    Stream.repeatEval(tick(commands, jobs).handleErrorWith(error => summon[Logger[F]].error(error)("Command scheduler tick failed")))
      .metered(1.second)
}
