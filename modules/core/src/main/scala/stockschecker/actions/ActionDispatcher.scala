package stockschecker.actions

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.{Job, JobId}
import stockschecker.repositories.JobRepository

trait ActionDispatcher[F[_]]:
  def dispatch(action: Action): F[Unit]

object ActionDispatcher:
  /** Every accepted action is durable; producers never wait on worker queue capacity. */
  def durable[F[_]](repository: JobRepository[F])(using F: Async[F], C: Clock[F]): F[ActionDispatcher[F]] =
    F.pure(new ActionDispatcher[F] {
      override def dispatch(action: Action): F[Unit] =
        for {
          now <- C.now
          id  <- F.delay(JobId.gen)
          _   <- repository.enqueue(Job(id, action, now, now))
        } yield ()
    })
