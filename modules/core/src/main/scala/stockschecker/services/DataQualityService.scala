package stockschecker.services

import cats.effect.Async
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.common.config.InvestmentConfig
import stockschecker.domain.*
import stockschecker.repositories.Repositories

trait DataQualityService[F[_]]:
  def report: F[DataQualityReport]

final private class LiveDataQualityService[F[_]](
    private val repos: Repositories[F],
    private val config: InvestmentConfig
)(using
    F: Async[F],
    C: Clock[F]
) extends DataQualityService[F] {
  override def report: F[DataQualityReport] =
    for
      at         <- C.now
      datasets   <- repos.dataQuality.all
      securities <- repos.security.streamAll.compile.toList
      ranking    <- repos.investmentRankings.latestHeader
      exclusions = securities.flatMap(s => s.exclusionReason.map(r => ExcludedSecurity(s.ticker, r, s.listingStatus)))
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
      config.enabled,
      config.coverageAudited,
      ranking.map(_.createdAt)
    )
}

object DataQualityService:
  def make[F[_]: {Async, Clock}](repos: Repositories[F], config: InvestmentConfig): F[DataQualityService[F]] =
    Async[F].pure(LiveDataQualityService[F](repos, config))
