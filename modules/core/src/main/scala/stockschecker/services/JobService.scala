package stockschecker.services

import cats.Applicative
import stockschecker.domain.{Job, JobFilter, JobId}
import stockschecker.repositories.JobRepository

trait JobService[F[_]]:
  def get(id: JobId): F[Job]
  def list(limit: Int, filter: JobFilter): F[List[Job]]

final private class LiveJobService[F[_]](
    private val repository: JobRepository[F]
) extends JobService[F] {
  override def get(id: JobId): F[Job] = repository.get(id)

  override def list(limit: Int, filter: JobFilter): F[List[Job]] = repository.list(limit, filter)
}

object JobService:
  def make[F[_]: Applicative](repository: JobRepository[F]): F[JobService[F]] =
    Applicative[F].pure(LiveJobService(repository))
