package stockschecker.repositories

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.apply.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Sort}
import stockschecker.domain.{Exchange, SecurityKind, Stock, StockFilters, StockSortField, Ticker}
import stockschecker.repositories.entities.StockEntity

trait StockRepository[F[_]]:
  def find(ticker: Ticker): F[Option[Stock]]
  def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]]
  def findByTickers(tickers: NonEmptyList[Ticker]): F[List[Stock]]

final private class LiveStockRepository[F[_]](
    private val collection: MongoCollection[F, StockEntity]
)(using
    F: Monad[F]
) extends StockRepository[F] {
  import StockRepository.Field

  private def buildAggregation(securityFilter: Filter, filters: StockFilters, limit: Int): Aggregate =
    val sortField = filters.sortBy match
      case Some(StockSortField.MarketCap)       => s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"
      case Some(StockSortField.OverallScore)    => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.OverallScore}"
      case Some(StockSortField.CagrScore)       => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.CagrScore}"
      case Some(StockSortField.VolatilityScore) => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.VolatilityScore}"
      case None                                 => s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"

    Aggregate
      .matchBy(securityFilter)
      .replaceWith(Document("security" := "$$ROOT"))
      .lookup(CompanyProfileRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.Profile)
      .lookup(PriceAnalyticsRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.PriceAnalytics)
      .lookup(FinancialMetricsRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.FinancialMetrics)
      .matchBy(filters.toProfileFilter && filters.toPriceAnalyticsFilter && filters.toFinancialMetricsFilter)
      .sort(Sort.desc(sortField))
      .limit(limit)

  override def find(ticker: Ticker): F[Option[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(Filter.idEq(ticker.value), StockFilters(), 1))
      .first
      .mapOpt(_.toDomain)

  override def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(filters.toSecurityFilter, filters, limit.getOrElse(Int.MaxValue)))
      .all
      .mapList(_.toDomain)

  override def findByTickers(tickers: NonEmptyList[Ticker]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(Filter.in(Field.Id, tickers.toList.map(_.value)), StockFilters(), tickers.size))
      .all
      .mapList(_.toDomain)

  extension (sf: StockFilters)
    private def toSecurityFilter: Filter = List(
      sf.exchange.map(e => Filter.eq(SecurityRepository.Field.Exchange, e)),
      sf.kind.map(k => Filter.eq(SecurityRepository.Field.Kind, k)),
      sf.isActive.map(a => Filter.eq(SecurityRepository.Field.IsActive, a))
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toProfileFilter: Filter = List(
      sf.country.map(c => Filter.eq(s"${StockRepository.Field.Profile}.0.${CompanyProfileRepository.Field.Country}", c)),
      sf.minMarketCap.map(min => Filter.gte(s"${StockRepository.Field.Profile}.0.${CompanyProfileRepository.Field.MarketCap}", min)),
      sf.maxMarketCap.map(max => Filter.lte(s"${StockRepository.Field.Profile}.0.${CompanyProfileRepository.Field.MarketCap}", max))
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toPriceAnalyticsFilter: Filter = List(
      sf.minPrice.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.LatestPrice}", min)
      ),
      sf.maxPrice.map(max =>
        Filter.lte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.LatestPrice}", max)
      ),
      (sf.minChange, sf.period).mapN { (min, period) =>
        Filter.gte(
          s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.Root}.${period.fieldName}",
          min
        )
      },
      (sf.maxChange, sf.period).mapN { (max, period) =>
        Filter.lte(
          s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.Root}.${period.fieldName}",
          max
        )
      },
      sf.minOverallScore.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.OverallScore}", min)
      ),
      sf.minCagrScore.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.CagrScore}", min)
      ),
      sf.minVolatilityScore.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.VolatilityScore}", min)
      ),
      sf.maxDrawdown.map(max =>
        Filter.lte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Metrics.Root}.maxDrawdown", max)
      ),
      sf.minConsistencyScore.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Metrics.Root}.consistencyScore", min)
      )
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toFinancialMetricsFilter: Filter = List(
      sf.minPE.map(min => Filter.gte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.PeRatioTtm}", min)),
      sf.maxPE.map(max => Filter.lte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.PeRatioTtm}", max)),
      sf.minROE.map(min => Filter.gte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.RoeTtm}", min)),
      sf.maxDebtToEquity.map(max =>
        Filter.lte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.DebtToEquityAnnual}", max)
      ),
      sf.minProfitMargin.map(min =>
        Filter.gte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.ProfitMarginTtm}", min)
      ),
      sf.maxProfitMargin.map(max =>
        Filter.lte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.ProfitMarginTtm}", max)
      ),
      sf.minFreeCashFlow.map(min =>
        Filter.gte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.FreeCashFlowPerShareTtm}", min)
      ),
      sf.minRevenueGrowth5Y.map(min =>
        Filter.gte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.RevenueGrowth5Y}", min)
      ),
      sf.minEpsGrowth5Y.map(min =>
        Filter.gte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.EpsGrowth5Y}", min)
      ),
      sf.minDividendYield.map(min =>
        Filter.gte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.DividendYieldAnnual}", min)
      ),
      sf.maxDividendYield.map(max =>
        Filter.lte(s"${StockRepository.Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.DividendYieldAnnual}", max)
      )
    ).flatten.foldLeft(Filter.empty)(_ && _)
}

object StockRepository extends MongoJsonCodecs:
  object Field:
    val Id               = "_id"
    val Profile          = "profile"
    val PriceAnalytics   = "priceAnalytics"
    val FinancialMetrics = "financialMetrics"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity](SecurityRepository.CollectionName)
      .map(_.withAddedCodec[Exchange].withAddedCodec[SecurityKind])
      .map(LiveStockRepository[F](_))
