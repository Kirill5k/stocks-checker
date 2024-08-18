package stockschecker.services

import cats.effect.kernel.Temporal
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import stockschecker.actions.Action
import stockschecker.actions.ActionDispatcher
import stockschecker.repositories.CommandRepository

trait CommandService[F[_]]:
  def rescheduleAll: F[Unit]

final private class LiveCommandService[F[_]](
    private val actionDispatcher: ActionDispatcher[F],
    private val repo: CommandRepository[F]
)(using 
  F: Temporal[F],
  C: Clock[F]
) extends CommandService[F] {
  
  override def rescheduleAll: F[Unit] =
    C.now.flatMap { now =>
      repo
        .streamActive
        .evalTap { cmd =>
          actionDispatcher.dispatch(Action.Schedule(cmd.id, cmd.durationUntilNextExecution(now)))
        }
        .compile
        .drain
    }
  
}

object CommandService:
  def make[F[_]: Temporal: Clock](ad: ActionDispatcher[F], repo: CommandRepository[F]): F[CommandService[F]] =
    Temporal[F].pure(LiveCommandService[F](ad, repo))