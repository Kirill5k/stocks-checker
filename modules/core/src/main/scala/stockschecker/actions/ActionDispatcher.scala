package stockschecker.actions

import cats.effect.Async
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import stockschecker.domain.{CommandId, Job, JobId}
import stockschecker.repositories.JobRepository

trait ActionDispatcher[F[_]]:
  def dispatch(action: Action): F[Unit]
  def dispatchOnce(id: JobId, action: Action): F[Unit]
  def pending(ids: List[JobId]): F[Set[JobId]]
  def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit]

final private class LiveActionDispatcher[F[_]](repository: JobRepository[F])(using F: Async[F], C: Clock[F]) extends ActionDispatcher[F]:
  override def dispatch(action: Action): F[Unit] =
    F.delay(JobId.gen).flatMap(dispatchOnce(_, action))

  override def dispatchOnce(id: JobId, action: Action): F[Unit] =
    C.now.flatMap(now => repository.enqueue(Job(id, action, now, now), ActionPriority.forAction(action)))

  override def pending(ids: List[JobId]): F[Set[JobId]] = repository.pendingIds(ids)

  override def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit] =
    repository.cancelObsolete(commandId, currentVersion)

object ActionDispatcher:
  /** Every accepted action is durable; producers never wait on worker queue capacity. */
  def make[F[_]: {Async, Clock}](repository: JobRepository[F]): F[ActionDispatcher[F]] =
    Async[F].pure(new LiveActionDispatcher(repository))
