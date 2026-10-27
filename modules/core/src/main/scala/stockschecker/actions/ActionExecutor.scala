package stockschecker.actions

import cats.effect.Async
import fs2.Stream
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.repositories.JobRepository
import stockschecker.domain.JobCommand

trait ActionExecutor[F[_]]:
  def run: Stream[F, Unit]

object ActionExecutor:
  def make[F[_]: {Async, Logger, Clock}](
      jobs: JobRepository[F],
      handler: ActionHandler[F],
      isCurrent: JobCommand => F[Boolean],
      config: ActionExecutorConfig
  ): F[ActionExecutor[F]] =
    Async[F].pure(new DurableActionExecutor(jobs, handler, isCurrent, config))
