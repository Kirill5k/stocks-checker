package stockschecker.domain

import cats.data.NonEmptyList
import org.latestbit.circe.adt.codec.JsonTaggedAdt

import java.time.LocalDate

import org.latestbit.circe.adt.codec.*

enum CompanyProfileFilter derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case MarketCapAbove(min: BigDecimal)
  case MarketCapBelow(min: BigDecimal)
  case CountryIs(countryCode: String)
  case IpoDateAfter(date: LocalDate)
  case IpoDateBefore(date: LocalDate)
  case Composite(filters: NonEmptyList[CompanyProfileFilter])
  case LastUpdatedAfter(date: LocalDate)
  case LastUpdatedBefore(date: LocalDate)

object CompanyProfileFilter {
  given JsonTaggedAdt.Config[CompanyProfileFilter] = JsonTaggedAdt.Config.Values[CompanyProfileFilter](
    mappings = Map(
      "market-cap-above"    -> JsonTaggedAdt.tagged[CompanyProfileFilter.MarketCapAbove],
      "market-cap-below"    -> JsonTaggedAdt.tagged[CompanyProfileFilter.MarketCapBelow],
      "country-is"          -> JsonTaggedAdt.tagged[CompanyProfileFilter.CountryIs],
      "ipo-date-after"      -> JsonTaggedAdt.tagged[CompanyProfileFilter.IpoDateAfter],
      "ipo-date-before"     -> JsonTaggedAdt.tagged[CompanyProfileFilter.IpoDateBefore],
      "last-updated-after"  -> JsonTaggedAdt.tagged[CompanyProfileFilter.LastUpdatedAfter],
      "last-updated-before" -> JsonTaggedAdt.tagged[CompanyProfileFilter.LastUpdatedBefore],
      "composite"           -> JsonTaggedAdt.tagged[CompanyProfileFilter.Composite]
    ),
    strict = true,
    typeFieldName = "kind"
  )
}
