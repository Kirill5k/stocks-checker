package stockschecker.actions

import cats.Functor
import cats.syntax.functor.*
import cats.effect.Concurrent
import cats.effect.std.Queue
import fs2.Stream

trait ActionDispatcher[F[_]]:
  def dispatch(action: Action): F[Unit]
  def pendingActions: Stream[F, Action]

final private class LiveActionDispatcher[F[_]: Functor](
    private val actions: Queue[F, Action]
) extends ActionDispatcher[F]:
  override def dispatch(action: Action): F[Unit] = actions.offer(action)
  override def pendingActions: Stream[F, Action] = Stream.fromQueueUnterminated(actions)

object ActionDispatcher:
  def make[F[_]](using F: Concurrent[F]): F[ActionDispatcher[F]] =
    Queue.bounded(128).map(LiveActionDispatcher[F](_))
