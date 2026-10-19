package stockschecker.domain

import cats.data.NonEmptyList
import io.circe.derivation.{Configuration, ConfiguredCodec}
import stockschecker.common.JsonCodecs

import java.time.LocalDate
import scala.concurrent.duration.FiniteDuration

enum CompanyProfileFilter derives ConfiguredCodec:
  case IsActive(active: Boolean)
  case MarketCapAbove(min: Long)
  case MarketCapBelow(max: Long)
  case CountryIs(countryCode: String)
  case IpoDateAfter(date: LocalDate)
  case IpoDateBefore(date: LocalDate)
  case Composite(filters: NonEmptyList[CompanyProfileFilter])
  case UpdatedWithin(duration: FiniteDuration)
  case NotUpdatedFor(duration: FiniteDuration)
  case PriceAnalyticsNotUpdatedFor(duration: FiniteDuration)
  case FinancialMetricsNotUpdatedFor(duration: FiniteDuration)
  case TickerMatching(pattern: String)

object CompanyProfileFilter extends JsonCodecs:
  given Configuration = Configuration.default.withKebabCaseConstructorNames.withDiscriminator("kind").withDefaults
