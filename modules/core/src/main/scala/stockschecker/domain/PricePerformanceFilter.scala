package stockschecker.domain

import cats.data.NonEmptyList
import org.latestbit.circe.adt.codec.{JsonTaggedAdt, *}
import stockschecker.common.JsonCodecs
import stockschecker.common.types.EnumType

import scala.concurrent.duration.FiniteDuration

object TimePeriod extends EnumType[TimePeriod](() => TimePeriod.values)
enum TimePeriod(val fieldName: String):
  case OneMonth extends TimePeriod("oneMonthChange")
  case ThreeMonth extends TimePeriod("threeMonthChange")
  case SixMonth extends TimePeriod("sixMonthChange")
  case OneYear extends TimePeriod("oneYearChange")
  case ThreeYear extends TimePeriod("threeYearChange")
  case FiveYear extends TimePeriod("fiveYearChange")
  case TenYear extends TimePeriod("tenYearChange")
  case Max extends TimePeriod("maxChange")

enum PricePerformanceFilter derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Composite(filters: NonEmptyList[SecurityFilter])
  case UpdatedWithin(duration: FiniteDuration)
  case NotUpdatedFor(duration: FiniteDuration)
  case PerformanceAbove(period: TimePeriod, minPercentage: BigDecimal)
  case PerformanceBelow(period: TimePeriod, maxPercentage: BigDecimal)

object PricePerformanceFilter extends JsonCodecs:
  given JsonTaggedAdt.Config[PricePerformanceFilter] = JsonTaggedAdt.Config.Values[PricePerformanceFilter](
    mappings = Map(
      "updated-within"  -> JsonTaggedAdt.tagged[PricePerformanceFilter.UpdatedWithin],
      "not-updated-for" -> JsonTaggedAdt.tagged[PricePerformanceFilter.NotUpdatedFor],
      "performance-above" -> JsonTaggedAdt.tagged[PricePerformanceFilter.PerformanceAbove],
      "performance-below" -> JsonTaggedAdt.tagged[PricePerformanceFilter.PerformanceBelow],
      "composite"       -> JsonTaggedAdt.tagged[PricePerformanceFilter.Composite]
    ),
    strict = true,
    typeFieldName = "kind"
  )