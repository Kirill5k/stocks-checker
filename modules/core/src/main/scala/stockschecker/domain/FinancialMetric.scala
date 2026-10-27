package stockschecker.domain

import stockschecker.common.types.EnumType

enum MetricUnit:
  case Ratio, PerShare, Multiple

object MetricUnit extends EnumType[MetricUnit](() => MetricUnit.values)

enum FinancialMetric(val unit: MetricUnit):
  case ReturnOnAssets          extends FinancialMetric(MetricUnit.Ratio)
  case FreeCashFlowMargin      extends FinancialMetric(MetricUnit.Ratio)
  case DebtToAssets            extends FinancialMetric(MetricUnit.Ratio)
  case FreeCashFlowPerShareTtm extends FinancialMetric(MetricUnit.PerShare)
  case PriceToEarningsTtm      extends FinancialMetric(MetricUnit.Multiple)
  case PriceToFreeCashFlowTtm  extends FinancialMetric(MetricUnit.Multiple)
  case EpsTtm                  extends FinancialMetric(MetricUnit.PerShare)
  case RevenueGrowth5Y         extends FinancialMetric(MetricUnit.Ratio)
  case EpsGrowth5Y             extends FinancialMetric(MetricUnit.Ratio)

object FinancialMetric extends EnumType[FinancialMetric](() => FinancialMetric.values)
