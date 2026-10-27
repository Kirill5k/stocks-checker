package stockschecker.clients

import cats.MonadThrow
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.Ticker
import stockschecker.utils.ProviderFailure
import stockschecker.domain.errors.AppError
import stockschecker.repositories.DataQualityRepository

/** Records one outcome for a logical dataset request, after any provider-account rotation. */
trait DatasetAvailabilityObserver[F[_]]:
  def observed[A](ticker: Ticker, provider: String, dataset: String)(request: F[A])(present: A => Boolean): F[A]

final private class LiveDatasetAvailabilityObserver[F[_]](
    private val quality: DataQualityRepository[F]
)(using F: MonadThrow[F], C: Clock[F])
    extends DatasetAvailabilityObserver[F] {
  override def observed[A](ticker: Ticker, provider: String, dataset: String)(request: F[A])(present: A => Boolean): F[A] =
    request.attempt.flatMap {
      case Right(value) => record(ticker, provider, dataset, if (present(value)) "available" else "unavailable", None).as(value)
      case Left(error: AppError.ProviderDeferred) => F.raiseError(error)
      case Left(error)                            =>
        val status = error match
          case _: AppError.ProviderConfiguration         => "configuration-error"
          case e: AppError.HttpClient if e.status == 404 => "unavailable"
          case _                                         => "failed"
        record(ticker, provider, dataset, status, Some(ProviderFailure.summary(error))) >> F.raiseError(error)
    }

  private def record(ticker: Ticker, provider: String, dataset: String, status: String, failure: Option[String]): F[Unit] =
    C.now.flatMap(now => quality.record(ticker, provider, dataset, status, now, failure))
}

object DatasetAvailabilityObserver:
  def make[F[_]: {MonadThrow, Clock}](quality: DataQualityRepository[F]): F[DatasetAvailabilityObserver[F]] =
    MonadThrow[F].pure(new LiveDatasetAvailabilityObserver(quality))
