package stockschecker.domain

import stockschecker.common.types.EnumType

/** Values retain the provider names used in stored observations and API responses. */
enum Provider:
  case Finnhub, TwelveData

object Provider extends EnumType[Provider](() => Provider.values, _.toString)
