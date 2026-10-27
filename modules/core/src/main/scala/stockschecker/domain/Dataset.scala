package stockschecker.domain

import stockschecker.common.types.EnumType

enum Dataset(val value: String):
  case CompanyProfile                extends Dataset("profile")
  case Financials                    extends Dataset("financials")
  case DailyPricesUnadjusted         extends Dataset("daily-prices-none")
  case DailyPricesSplits             extends Dataset("daily-prices-splits")
  case DailyPricesSplitsAndDividends extends Dataset("daily-prices-all")

object Dataset extends EnumType[Dataset](() => Dataset.values, _.value):
  def dailyPrices(adjustment: PriceAdjustment): Dataset = adjustment match
    case PriceAdjustment.Unadjusted         => DailyPricesUnadjusted
    case PriceAdjustment.Splits             => DailyPricesSplits
    case PriceAdjustment.SplitsAndDividends => DailyPricesSplitsAndDividends
