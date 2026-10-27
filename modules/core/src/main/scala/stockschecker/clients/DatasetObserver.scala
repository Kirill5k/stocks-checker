package stockschecker.clients

import cats.effect.Sync
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.DataQualityRepository

private[clients] trait DatasetObserver[F[_]]:
  def observe[A](provider: Provider, dataset: Dataset, ticker: Ticker)(request: F[A])(present: A => Boolean): F[A]

final private class LiveDatasetObserver[F[_]](
    quality: DataQualityRepository[F]
)(using F: Sync[F], C: Clock[F])
    extends DatasetObserver[F]:
  override def observe[A](provider: Provider, dataset: Dataset, ticker: Ticker)(request: F[A])(present: A => Boolean): F[A] =
    def record(status: DatasetStatus, failure: Option[String]): F[Unit] =
      C.now.flatMap(now => quality.record(ticker, provider, dataset, status, now, failure))

    request.attempt.flatMap {
      case Right(value) =>
        record(if (present(value)) DatasetStatus.Available else DatasetStatus.Unavailable, None).as(value)
      case Left(error: AppError.ProviderDeferred) => F.raiseError(error)
      case Left(error)                            =>
        val status = error match
          case _: AppError.ProviderConfiguration         => DatasetStatus.ConfigurationError
          case e: AppError.HttpClient if e.status == 404 => DatasetStatus.Unavailable
          case _                                         => DatasetStatus.Failed
        record(status, Some(AppError.safeSummary(error))) >> F.raiseError(error)
    }

private[clients] object DatasetObserver:
  def make[F[_]: {Sync, Clock}](quality: DataQualityRepository[F]): F[DatasetObserver[F]] =
    Sync[F].pure(new LiveDatasetObserver[F](quality))
