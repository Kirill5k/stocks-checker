package stockschecker.domain

import stockschecker.common.types.EnumType
import scala.concurrent.duration.*

/** Values retain the provider names used in stored observations and API responses. */
enum Provider:
  case Finnhub, TwelveData

object Provider extends EnumType[Provider](() => Provider.values, _.toString):
  val configurationRetryDelay: FiniteDuration = 15.minutes
