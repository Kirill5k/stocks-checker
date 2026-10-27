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
import stockschecker.repositories.{CommandRepository, JobRepository}

import scala.concurrent.duration.*

/** Scheduling and outbox recovery never share the execution worker pool. */
object CommandScheduler {
  def tick[F[_]: {Temporal, Clock}](commands: CommandRepository[F], jobs: JobRepository[F]): F[Unit] =
    for {
      now       <- summon[Clock[F]].now
      pending   <- commands.pending(now)
      completed <- jobs.terminalIds(pending.flatMap(_.activeRunId))
      results   <- pending.traverse { command =>
        (for {
          _ <- command.outbox.traverse_ { occurrence =>
            jobs.enqueue(occurrence.job, ActionPriority.forAction(occurrence.job.action)) >> commands.clearOutbox(
              command.id,
              occurrence.job.id
            )
          }
          current <- command.activeRunId
            .filter(completed)
            .fold(Option(command.copy(outbox = None)).pure[F])(id => commands.completeRun(command.id, id))
          ready      <- current.traverse(commands.initializeNextRun(_, now)).map(_.flatten)
          occurrence <- ready.traverse(commands.reserve(_, now)).map(_.flatten)
          _          <- occurrence.traverse_(o =>
            jobs.enqueue(o.job, ActionPriority.forAction(o.job.action)) >> commands.clearOutbox(command.id, o.job.id)
          )
        } yield ()).attempt
      }
      _ <- results.collectFirst { case Left(error) => error }.fold(Temporal[F].unit)(Temporal[F].raiseError[Unit])
    } yield ()

  def run[F[_]: {Temporal, Clock, Logger}](commands: CommandRepository[F], jobs: JobRepository[F]): Stream[F, Unit] =
    Stream
      .repeatEval(tick(commands, jobs).handleErrorWith(error => summon[Logger[F]].error(error)("Command scheduler tick failed")))
      .metered(1.second)
}
