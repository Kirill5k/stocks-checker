package stockschecker.services

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.applicativeError.*
import stockschecker.domain.errors.AppError
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.repositories.{DataQualityRepository, SecurityRepository}

trait DataQualityService[F[_]]:
  def observe[A](ticker: Ticker, provider: Provider, dataset: String)(request: F[A])(present: A => Boolean): F[A]
  def report: F[DataQualityReport]

final private class LiveDataQualityService[F[_]](
    private val quality: DataQualityRepository[F],
    private val securities: SecurityRepository[F],
    private val rankings: InvestmentRankingService[F]
)(using
    F: Async[F],
    C: Clock[F]
) extends DataQualityService[F] {
  override def observe[A](ticker: Ticker, provider: Provider, dataset: String)(request: F[A])(present: A => Boolean): F[A] =
    request.attempt.flatMap {
      case Right(value) =>
        record(ticker, provider, dataset, if (present(value)) DatasetStatus.Available else DatasetStatus.Unavailable, None).as(value)
      case Left(error: AppError.ProviderDeferred) => F.raiseError(error)
      case Left(error)                            =>
        val status = error match
          case _: AppError.ProviderConfiguration         => DatasetStatus.ConfigurationError
          case e: AppError.HttpClient if e.status == 404 => DatasetStatus.Unavailable
          case _                                         => DatasetStatus.Failed
        record(ticker, provider, dataset, status, Some(AppError.safeSummary(error))) >> F.raiseError(error)
    }

  private def record(ticker: Ticker, provider: Provider, dataset: String, status: DatasetStatus, failure: Option[String]): F[Unit] =
    C.now.flatMap(now => quality.record(ticker, provider, dataset, status, now, failure))

  override def report: F[DataQualityReport] =
    for
      at       <- C.now
      datasets <- quality.all
      excluded <- securities.findExcluded
      ranking  <- rankings.latestAvailable
      exclusions = excluded.flatMap(s => s.exclusionReason.map(r => ExcludedSecurity(s.ticker, r, s.listingStatus)))
      coverage   = ranking
        .map { header =>
          header.summary.excludedByReason ++ Map("eligible" -> header.summary.eligibleCount, "universe" -> header.summary.universeCount)
        }
        .getOrElse(Map.empty)
    yield DataQualityReport(
      at,
      datasets.sortBy(d => (d.ticker.value, d.dataset)),
      exclusions,
      datasets.groupMapReduce(_.status)(_ => 1)(_ + _),
      coverage,
      ranking.map(_.createdAt)
    )
}

object DataQualityService:
  def make[F[_]: {Async, Clock}](
      quality: DataQualityRepository[F],
      securities: SecurityRepository[F],
      rankings: InvestmentRankingService[F]
  ): F[DataQualityService[F]] =
    Async[F].pure(LiveDataQualityService[F](quality, securities, rankings))
