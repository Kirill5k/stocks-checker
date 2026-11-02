package stockschecker.actions

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.{CommandId, Job, JobFilter, JobId}
import stockschecker.repositories.JobRepository

trait ActionDispatcher[F[_]]:
  def dispatch(action: Action): F[Unit]
  def dispatchTracked(action: Action): F[JobId]
  def dispatchOnce(id: JobId, action: Action): F[Unit]
  def findJob(id: JobId): F[Option[Job]]
  def listJobs(limit: Int, filter: JobFilter): F[List[Job]]
  def pending(ids: List[JobId]): F[Set[JobId]]
  def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit]

final private class LiveActionDispatcher[F[_]](repository: JobRepository[F])(using F: Async[F], C: Clock[F]) extends ActionDispatcher[F]:
  override def dispatch(action: Action): F[Unit] =
    dispatchTracked(action).void

  override def dispatchTracked(action: Action): F[JobId] =
    F.delay(JobId.gen).flatTap(dispatchOnce(_, action))

  override def dispatchOnce(id: JobId, action: Action): F[Unit] =
    C.now.flatMap(now => repository.enqueue(Job(id, action, now, now), ActionPriority.forAction(action)))

  override def pending(ids: List[JobId]): F[Set[JobId]] = repository.pendingIds(ids)

  override def findJob(id: JobId): F[Option[Job]] = repository.find(id)

  override def listJobs(limit: Int, filter: JobFilter): F[List[Job]] = repository.list(limit, filter)

  override def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit] =
    repository.cancelObsolete(commandId, currentVersion)

object ActionDispatcher:
  /** Every accepted action is durable; producers never wait on worker queue capacity. */
  def make[F[_]: {Async, Clock}](repository: JobRepository[F]): F[ActionDispatcher[F]] =
    Async[F].pure(new LiveActionDispatcher(repository))
