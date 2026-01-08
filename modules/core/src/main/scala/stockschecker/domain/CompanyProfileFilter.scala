package stockschecker.domain

import cats.data.NonEmptyList
import org.latestbit.circe.adt.codec.*
import stockschecker.common.JsonCodecs

import java.time.LocalDate
import scala.concurrent.duration.FiniteDuration

enum CompanyProfileFilter derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case MarketCapAbove(min: Long)
  case MarketCapBelow(min: Long)
  case CountryIs(countryCode: String)
  case IpoDateAfter(date: LocalDate)
  case IpoDateBefore(date: LocalDate)
  case Composite(filters: NonEmptyList[CompanyProfileFilter])
  case UpdatedWithin(duration: FiniteDuration)
  case NotUpdatedFor(duration: FiniteDuration)
  case PriceAnalyticsNotUpdatedFor(duration: FiniteDuration)
  case TickerMatching(pattern: String)

object CompanyProfileFilter extends JsonCodecs:
  given JsonTaggedAdt.Config[CompanyProfileFilter] = JsonTaggedAdt.Config.Values[CompanyProfileFilter](
    mappings = Map(
      "market-cap-above"                  -> JsonTaggedAdt.tagged[CompanyProfileFilter.MarketCapAbove],
      "market-cap-below"                  -> JsonTaggedAdt.tagged[CompanyProfileFilter.MarketCapBelow],
      "country-is"                        -> JsonTaggedAdt.tagged[CompanyProfileFilter.CountryIs],
      "ipo-date-after"                    -> JsonTaggedAdt.tagged[CompanyProfileFilter.IpoDateAfter],
      "ipo-date-before"                   -> JsonTaggedAdt.tagged[CompanyProfileFilter.IpoDateBefore],
      "updated-within"                    -> JsonTaggedAdt.tagged[CompanyProfileFilter.UpdatedWithin],
      "not-updated-for"                   -> JsonTaggedAdt.tagged[CompanyProfileFilter.NotUpdatedFor],
      "price-analytics-not-updated-for"   -> JsonTaggedAdt.tagged[CompanyProfileFilter.PriceAnalyticsNotUpdatedFor],
      "ticker-matching"                   -> JsonTaggedAdt.tagged[CompanyProfileFilter.TickerMatching],
      "composite"                         -> JsonTaggedAdt.tagged[CompanyProfileFilter.Composite]
    ),
    strict = true,
    typeFieldName = "kind"
  )
