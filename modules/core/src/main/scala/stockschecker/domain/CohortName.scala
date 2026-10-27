package stockschecker.domain

import stockschecker.common.types.EnumType

enum CohortName:
  case Investment, OldPriceScore, Quality, Value, EqualUniverse, Vti

object CohortName extends EnumType[CohortName](() => CohortName.values)
