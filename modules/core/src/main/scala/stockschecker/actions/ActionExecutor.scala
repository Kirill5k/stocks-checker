package stockschecker.actions

import cats.effect.Async
import fs2.Stream
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.repositories.JobRepository
import stockschecker.services.Services

trait ActionExecutor[F[_]]:
  def run: Stream[F, Unit]

object ActionExecutor:
  def make[F[_]: {Async, Logger, Clock}](
      jobs: JobRepository[F],
      services: Services[F],
      config: ActionExecutorConfig
  ): F[ActionExecutor[F]] =
    Async[F].pure(new DurableActionExecutor(jobs, services, config))
