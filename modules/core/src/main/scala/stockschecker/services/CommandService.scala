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
    private val commandRepository: CommandRepository[F]
)(using 
  F: Temporal[F],
  C: Clock[F]
) extends CommandService[F] {
  
  override def rescheduleAll: F[Unit] =
    C.now.flatMap { now =>
      commandRepository
        .streamActive
        .evalTap { cmd =>
          actionDispatcher.dispatch(Action.Schedule(cmd.id, cmd.durationUntilNextExecution(now)))
        }
        .compile
        .drain
    }
  
}
