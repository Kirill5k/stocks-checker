package stockschecker.domain

import cats.data.NonEmptyList
import org.latestbit.circe.adt.codec.{JsonTaggedAdt, *}
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

enum PriceAnalyticsFilter derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Composite(filters: NonEmptyList[PriceAnalyticsFilter])
  case UpdatedWithin(duration: FiniteDuration)
  case NotUpdatedFor(duration: FiniteDuration)
  case PriceAbove(minPrice: BigDecimal)
  case PriceBelow(maxPrice: BigDecimal)
  case PerformanceAbove(period: TimePeriod, minPercentage: BigDecimal)
  case PerformanceBelow(period: TimePeriod, maxPercentage: BigDecimal)

object PriceAnalyticsFilter extends JsonCodecs:
  given JsonTaggedAdt.Config[PriceAnalyticsFilter] = JsonTaggedAdt.Config.Values[PriceAnalyticsFilter](
    mappings = Map(
      "composite"         -> JsonTaggedAdt.tagged[PriceAnalyticsFilter.Composite],
      "updated-within"    -> JsonTaggedAdt.tagged[PriceAnalyticsFilter.UpdatedWithin],
      "not-updated-for"   -> JsonTaggedAdt.tagged[PriceAnalyticsFilter.NotUpdatedFor],
      "price-above"       -> JsonTaggedAdt.tagged[PriceAnalyticsFilter.PriceAbove],
      "price-below"       -> JsonTaggedAdt.tagged[PriceAnalyticsFilter.PriceBelow],
      "performance-above" -> JsonTaggedAdt.tagged[PriceAnalyticsFilter.PerformanceAbove],
      "performance-below" -> JsonTaggedAdt.tagged[PriceAnalyticsFilter.PerformanceBelow]
    ),
    strict = true,
    typeFieldName = "kind"
  )
