package stockschecker.domain

import cats.data.NonEmptyList
import io.circe.derivation.{Configuration, ConfiguredCodec}
import stockschecker.common.JsonCodecs
import stockschecker.common.types.EnumType

import scala.concurrent.duration.FiniteDuration

object TimePeriod extends EnumType[TimePeriod](() => TimePeriod.values)
enum TimePeriod(val fieldName: String):
  case OneMonth   extends TimePeriod("oneMonthChange")
  case ThreeMonth extends TimePeriod("threeMonthChange")
  case SixMonth   extends TimePeriod("sixMonthChange")
  case OneYear    extends TimePeriod("oneYearChange")
  case ThreeYear  extends TimePeriod("threeYearChange")
  case FiveYear   extends TimePeriod("fiveYearChange")
  case TenYear    extends TimePeriod("tenYearChange")
  case Max        extends TimePeriod("maxChange")

enum PriceAnalyticsFilter derives ConfiguredCodec:
  case Composite(filters: NonEmptyList[PriceAnalyticsFilter])
  case UpdatedWithin(duration: FiniteDuration)
  case NotUpdatedFor(duration: FiniteDuration)
  case PriceAbove(minPrice: BigDecimal)
  case PriceBelow(maxPrice: BigDecimal)
  case PerformanceAbove(period: TimePeriod, minPercentage: BigDecimal)
  case PerformanceBelow(period: TimePeriod, maxPercentage: BigDecimal)

object PriceAnalyticsFilter extends JsonCodecs:
  given Configuration = Configuration.default.withKebabCaseConstructorNames.withDiscriminator("kind").withDefaults
