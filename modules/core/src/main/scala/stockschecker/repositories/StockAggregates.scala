package stockschecker.repositories

import cats.syntax.apply.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.{Aggregate, Filter, Sort}
import stockschecker.domain.{StockFilters, StockSortField}

/** Shared stock joins; each requesting repository executes the aggregate against securities. */
private[repositories] object StockAggregates extends MongoJsonCodecs:
  object Field:
    val Security         = "security"
    val Id               = "_id"
    val Profile          = "profile"
    val PriceAnalytics   = "priceAnalytics"
    val FinancialMetrics = "financialMetrics"

  def build(securityFilter: Filter, filters: StockFilters, limit: Option[Int]): Aggregate =
    val sortField = filters.sortBy match
      case Some(StockSortField.MarketCap)       => s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"
      case Some(StockSortField.OverallScore)    => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.OverallScore}"
      case Some(StockSortField.CagrScore)       => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.CagrScore}"
      case Some(StockSortField.VolatilityScore) => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.VolatilityScore}"
      case None                                 => s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"

    val joined = Aggregate
      .matchBy(securityFilter)
      .replaceWith(Document(Field.Security := "$$ROOT"))
      .lookup(CompanyProfileRepository.CollectionName, s"${Field.Security}.${Field.Id}", Field.Id, Field.Profile)
      .matchBy(filters.toProfileFilter)
      .lookup(PriceAnalyticsRepository.CollectionName, s"${Field.Security}.${Field.Id}", Field.Id, Field.PriceAnalytics)
      .matchBy(filters.toPriceAnalyticsFilter)
      .lookup(FinancialMetricsRepository.CollectionName, s"${Field.Security}.${Field.Id}", Field.Id, Field.FinancialMetrics)
      .matchBy(filters.toFinancialMetricsFilter)
    limit.fold(joined)(n => joined.sort(Sort.desc(sortField)).limit(n))

  extension (sf: StockFilters)
    def toSecurityFilter: Filter = List(
      sf.exchange.map(e => Filter.eq(SecurityRepository.Field.Exchange, e)),
      sf.kind.map(k => Filter.eq(SecurityRepository.Field.Kind, k)),
      sf.isActive.map(a => Filter.eq(SecurityRepository.Field.IsActive, a))
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toProfileFilter: Filter = List(
      sf.country.map(c => Filter.eq(s"${Field.Profile}.0.${CompanyProfileRepository.Field.Country}", c)),
      sf.minMarketCap.map(min => Filter.gte(s"${Field.Profile}.0.${CompanyProfileRepository.Field.MarketCap}", min)),
      sf.maxMarketCap.map(max => Filter.lte(s"${Field.Profile}.0.${CompanyProfileRepository.Field.MarketCap}", max))
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toPriceAnalyticsFilter: Filter = List(
      sf.minPrice.map(min =>
        Filter.gte(s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.LatestPrice}", min)
      ),
      sf.maxPrice.map(max =>
        Filter.lte(s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.LatestPrice}", max)
      ),
      (sf.minChange, sf.period).mapN { (min, period) =>
        Filter.gte(
          s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.Root}.${period.fieldName}",
          min
        )
      },
      (sf.maxChange, sf.period).mapN { (max, period) =>
        Filter.lte(
          s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.Root}.${period.fieldName}",
          max
        )
      },
      sf.minOverallScore.map(min => Filter.gte(s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.OverallScore}", min)),
      sf.minCagrScore.map(min => Filter.gte(s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.CagrScore}", min)),
      sf.minVolatilityScore.map(min =>
        Filter.gte(s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.VolatilityScore}", min)
      ),
      sf.maxDrawdown.map(max => Filter.lte(s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Metrics.Root}.maxDrawdown", max)),
      sf.minConsistencyScore.map(min =>
        Filter.gte(s"${Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.Root}.consistencyScore", min)
      )
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toFinancialMetricsFilter: Filter = List(
      sf.minPE.map(min => Filter.gte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.PeRatioTtm}", min)),
      sf.maxPE.map(max => Filter.lte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.PeRatioTtm}", max)),
      sf.minROE.map(min => Filter.gte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.RoeTtm}", min)),
      sf.maxDebtToEquity.map(max => Filter.lte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.DebtToEquityAnnual}", max)),
      sf.minProfitMargin.map(min => Filter.gte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.ProfitMarginTtm}", min)),
      sf.maxProfitMargin.map(max => Filter.lte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.ProfitMarginTtm}", max)),
      sf.minFreeCashFlow.map(min =>
        Filter.gte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.FreeCashFlowPerShareTtm}", min)
      ),
      sf.minRevenueGrowth5Y.map(min => Filter.gte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.RevenueGrowth5Y}", min)),
      sf.minEpsGrowth5Y.map(min => Filter.gte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.EpsGrowth5Y}", min)),
      sf.minDividendYield.map(min =>
        Filter.gte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.DividendYieldAnnual}", min)
      ),
      sf.maxDividendYield.map(max =>
        Filter.lte(s"${Field.FinancialMetrics}.0.${FinancialMetricsRepository.Field.DividendYieldAnnual}", max)
      )
    ).flatten.foldLeft(Filter.empty)(_ && _)
