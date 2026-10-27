package stockschecker.domain

import stockschecker.common.types.EnumType

enum ScoringFeature:
  case ReturnOnAssets5YMedian, FreeCashFlowMargin5YMedian, EarningsYield, FreeCashFlowYield
  case RevenueGrowth5Y, EpsGrowth5Y, DebtToAssets, EpsTtm

object ScoringFeature extends EnumType[ScoringFeature](
      () => ScoringFeature.values,
      feature => {
        val name = feature.toString
        name.head.toLower.toString + name.tail
      }
    )
