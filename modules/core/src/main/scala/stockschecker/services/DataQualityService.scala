package stockschecker.services

import cats.effect.Sync
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.common.config.InvestmentConfig
import stockschecker.domain.*
import stockschecker.repositories.Repositories

trait DataQualityService[F[_]]:
  def report: F[DataQualityReport]

object DataQualityService {
  def make[F[_]: {Sync, Clock}](repos: Repositories[F], config: InvestmentConfig): F[DataQualityService[F]] =
    Sync[F].pure(new DataQualityService[F] {
      def report: F[DataQualityReport] =
        for
          at <- Clock[F].now
          datasets <- repos.dataQuality.all
          stocks <- repos.stock.findAll(StockFilters(), None)
          snapshots <- repos.financialHistory.findLatestAll
          byTicker = snapshots.map(s => s.ticker -> s).toMap
          cards = InvestmentScorer.rank(stocks.map(s => InvestmentCandidate(s, byTicker.get(s.security.ticker))), at,
            InvestmentRecipe(maxSnapshotAgeDays = config.snapshotFreshnessDays, maxAnnualAgeDays = config.annualFreshnessDays,
              maxPriceAgeDays = config.priceFreshnessDays, maxQuarterlyAgeDays = config.quarterlyFreshnessDays))
          exclusions = stocks.flatMap(s => s.security.exclusionReason.map(r => ExcludedSecurity(s.security.ticker, r, s.security.listingStatus)))
        yield DataQualityReport(at, datasets.sortBy(d => (d.ticker.value, d.dataset)), exclusions,
          datasets.groupMapReduce(_.status)(_ => 1)(_ + _),
          cards.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _) ++ Map("eligible" -> cards.count(_.eligible), "universe" -> stocks.size),
          config.enabled, config.coverageAudited)
    })
}
