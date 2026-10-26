package stockschecker.actions

import cats.effect.Async
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.domain.Job
import stockschecker.repositories.JobRepository

import java.util.UUID

trait ActionDispatcher[F[_]]:
  def dispatch(action: Action): F[Unit]

object ActionDispatcher:
  /** Every accepted action is durable; producers never wait on worker queue capacity. */
  def durable[F[_]: {Async, Clock}](repository: JobRepository[F]): F[ActionDispatcher[F]] =
    Async[F].pure(new ActionDispatcher[F] {
      override def dispatch(action: Action): F[Unit] =
        for {
          now <- summon[Clock[F]].now
          id  <- Async[F].delay(UUID.randomUUID().toString)
          _   <- repository.enqueue(Job(id, action, now, now))
        } yield ()
    })
