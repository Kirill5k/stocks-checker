package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.actions.ActionDispatcher
import stockschecker.domain.{JobFilter, JobId, JobView}
import stockschecker.domain.errors.AppError

trait JobService[F[_]]:
  def get(id: JobId): F[JobView]
  def list(limit: Int, filter: JobFilter): F[List[JobView]]

final private class LiveJobService[F[_]](dispatcher: ActionDispatcher[F])(using F: MonadThrow[F]) extends JobService[F]:
  override def get(id: JobId): F[JobView] =
    dispatcher.findJob(id).flatMap(job => F.fromOption(job, AppError.EntityDoesNotExist("Job", id.value))).map(JobView.from)

  override def list(limit: Int, filter: JobFilter): F[List[JobView]] =
    dispatcher.listJobs(limit, filter).map(_.map(JobView.from))

object JobService:
  def make[F[_]: MonadThrow](dispatcher: ActionDispatcher[F]): F[JobService[F]] =
    MonadThrow[F].pure(LiveJobService(dispatcher))
