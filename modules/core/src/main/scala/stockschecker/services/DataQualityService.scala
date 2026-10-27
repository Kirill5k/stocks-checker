package stockschecker.services

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.repositories.{DataQualityRepository, SecurityRepository}

trait DataQualityService[F[_]]:
  def report: F[DataQualityReport]

final private class LiveDataQualityService[F[_]](
    private val quality: DataQualityRepository[F],
    private val securities: SecurityRepository[F],
    private val rankings: InvestmentRankingService[F]
)(using
    F: Async[F],
    C: Clock[F]
) extends DataQualityService[F] {
  override def report: F[DataQualityReport] =
    for
      at       <- C.now
      datasets <- quality.all
      excluded <- securities.findExcluded
      ranking  <- rankings.latestHeader
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
