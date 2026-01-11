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
    maxPE: Option[BigDecimal] = None,
    minROE: Option[BigDecimal] = None,
    maxDebtToEquity: Option[BigDecimal] = None,
    sortBy: Option[StockSortField] = None
)
