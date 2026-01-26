package stockschecker.domain

import stockschecker.common.types.EnumType

object StockSortField extends EnumType[StockSortField](() => StockSortField.values)
enum StockSortField:
  case MarketCap
  case OverallScore
  case CagrScore
  case VolatilityScore

final case class StockFilters(
    exchange: Option[Exchange] = None,
    kind: Option[SecurityKind] = None,
    isActive: Option[Boolean] = None,
    country: Option[String] = None,
    minMarketCap: Option[Long] = None,
    maxMarketCap: Option[Long] = None,
    minPrice: Option[BigDecimal] = None,
    maxPrice: Option[BigDecimal] = None,
    minChange: Option[BigDecimal] = None,
    maxChange: Option[BigDecimal] = None,
    period: Option[TimePeriod] = None,
    minOverallScore: Option[BigDecimal] = None,
    minCagrScore: Option[BigDecimal] = None,
    minVolatilityScore: Option[BigDecimal] = None,
    minPE: Option[BigDecimal] = None,
    maxPE: Option[BigDecimal] = None,
    minROE: Option[BigDecimal] = None,
    maxDebtToEquity: Option[BigDecimal] = None,
    minProfitMargin: Option[BigDecimal] = None,
    maxProfitMargin: Option[BigDecimal] = None,
    minFreeCashFlow: Option[BigDecimal] = None,
    minRevenueGrowth5Y: Option[BigDecimal] = None,
    minEpsGrowth5Y: Option[BigDecimal] = None,
    minDividendYield: Option[BigDecimal] = None,
    maxDividendYield: Option[BigDecimal] = None,
    maxDrawdown: Option[BigDecimal] = None,
    minConsistencyScore: Option[BigDecimal] = None,
    sortBy: Option[StockSortField] = None
)
