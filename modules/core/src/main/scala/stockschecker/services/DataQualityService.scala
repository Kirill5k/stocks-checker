package stockschecker.services

import cats.effect.Async
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.common.config.InvestmentConfig
import stockschecker.domain.*
import stockschecker.repositories.Repositories

trait DataQualityService[F[_]]:
  def report: F[DataQualityReport]

object DataQualityService {
  def make[F[_]: {Async, Clock}](repos: Repositories[F], config: InvestmentConfig): F[DataQualityService[F]] =
    Async[F].pure(new DataQualityService[F] {
      def report: F[DataQualityReport] =
        for
          at         <- Clock[F].now
          datasets   <- repos.dataQuality.all
          securities <- repos.security.streamAll.compile.toList
          ranking    <- repos.investmentRankings.latestHeader
          exclusions = securities.flatMap(s => s.exclusionReason.map(r => ExcludedSecurity(s.ticker, r, s.listingStatus)))
          coverage   = ranking.fold(Map.empty[String, Int]) { header =>
            header.summary.excludedByReason ++ Map("eligible" -> header.summary.eligibleCount, "universe" -> header.summary.universeCount)
          }
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
    })
}
