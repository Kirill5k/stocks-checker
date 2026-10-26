package stockschecker.actions

import cats.Functor
import cats.syntax.functor.*
import cats.effect.{Async, Concurrent}
import cats.effect.std.Queue
import cats.syntax.flatMap.*
import fs2.Stream
import kirill5k.common.cats.Clock
import stockschecker.domain.Job
import stockschecker.repositories.JobRepository

import java.util.UUID

trait ActionDispatcher[F[_]]:
  def dispatch(action: Action): F[Unit]
  def pendingActions: Stream[F, Action]
  def jobRepository: Option[JobRepository[F]] = None

final private class LiveActionDispatcher[F[_]: Functor](
    private val actions: Queue[F, Action]
) extends ActionDispatcher[F]:
  override def dispatch(action: Action): F[Unit] = actions.offer(action)
  override def pendingActions: Stream[F, Action] = Stream.fromQueueUnterminated(actions)

object ActionDispatcher:
  def make[F[_]](using F: Concurrent[F]): F[ActionDispatcher[F]] =
    Queue.bounded(128).map(LiveActionDispatcher[F](_))

  /** Production dispatcher. Every accepted action is durable; producers never wait on worker queue capacity. */
  def durable[F[_]: {Async, Clock}](repository: JobRepository[F]): F[ActionDispatcher[F]] =
    Async[F].pure(new ActionDispatcher[F] {
      override val jobRepository: Option[JobRepository[F]] = Some(repository)
      override def dispatch(action: Action): F[Unit]       =
        for {
          now <- summon[Clock[F]].now
          id  <- Async[F].delay(UUID.randomUUID().toString)
          due = action match {
            case Action.Schedule(_, duration) => now.plusMillis(duration.toMillis)
            case _                            => now
          }
          _ <- repository.enqueue(Job(id, action, due, now))
        } yield ()
      override def pendingActions: Stream[F, Action] = Stream.evalSeq(repository.list()).map(_.action)
    })
