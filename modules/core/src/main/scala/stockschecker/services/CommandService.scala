package stockschecker.services

import cats.effect.kernel.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.actions.Action
import stockschecker.actions.ActionDispatcher
import stockschecker.domain.{Command, CommandId, CreateCommand}
import stockschecker.repositories.CommandRepository

trait CommandService[F[_]]:
  def rescheduleAll: F[Unit]
  def create(cmd: CreateCommand): F[Command]
  def execute(cid: CommandId): F[Unit]

final private class LiveCommandService[F[_]](
    private val actionDispatcher: ActionDispatcher[F],
    private val repo: CommandRepository[F]
)(using
    F: Temporal[F],
    C: Clock[F]
) extends CommandService[F] {

  override def rescheduleAll: F[Unit] =
    C.now.flatMap { now =>
      repo.streamActive
        .evalTap { cmd =>
          actionDispatcher.dispatch(Action.Schedule(cmd.id, cmd.durationUntilNextExecution(now)))
        }
        .compile
        .drain
    }

  override def create(cmd: CreateCommand): F[Command] =
    repo.create(cmd)

  override def execute(cid: CommandId): F[Unit] =
    for
      now <- C.now
      cmd <- repo.find(cid)
      _ <- F.whenA(cmd.canBeExecuted) {
        actionDispatcher.dispatch(cmd.action) >>
          repo.update(cmd.incExecutionCount(now))
      }
    yield ()
}

object CommandService:
  def make[F[_]: Temporal: Clock](ad: ActionDispatcher[F], repo: CommandRepository[F]): F[CommandService[F]] =
    Temporal[F].pure(LiveCommandService[F](ad, repo))
