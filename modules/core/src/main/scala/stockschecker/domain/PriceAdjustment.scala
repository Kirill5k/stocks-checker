package stockschecker.domain

import stockschecker.common.types.EnumType

/** Corporate-action adjustments applied to historical prices. Values match stored snapshot metadata. */
enum PriceAdjustment(val value: String):
  case Unadjusted extends PriceAdjustment("none")
  case Splits extends PriceAdjustment("splits")
  case SplitsAndDividends extends PriceAdjustment("split-and-dividend")

object PriceAdjustment extends EnumType[PriceAdjustment](() => PriceAdjustment.values, _.value)
