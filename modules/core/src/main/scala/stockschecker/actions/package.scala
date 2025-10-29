package stockschecker

import cats.data.NonEmptyList
import stockschecker.common.JsonCodecs
import stockschecker.domain.{CommandId, Exchange, Ticker}
import org.latestbit.circe.adt.codec.*

import java.time.LocalDate
import scala.concurrent.duration.FiniteDuration

package object actions extends JsonCodecs {

  enum RateLimitPolicy derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
    case PerMinute(requests: Int)
    case PerDay(requests: Int)
    case Unrestricted

  object RateLimitPolicy {
    given JsonTaggedAdt.Config[RateLimitPolicy] = JsonTaggedAdt.Config.Values[RateLimitPolicy](
      mappings = Map(
        "per-minute"   -> JsonTaggedAdt.tagged[RateLimitPolicy.PerMinute],
        "per-day"      -> JsonTaggedAdt.tagged[RateLimitPolicy.PerDay],
        "unrestricted" -> JsonTaggedAdt.tagged[RateLimitPolicy.Unrestricted.type]
      ),
      strict = true,
      typeFieldName = "kind"
    )
  }

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

  enum Action derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
    case RescheduleAll
    case Schedule(cid: CommandId, duration: FiniteDuration)

    case DiscoverSecurities(exchanges: NonEmptyList[Exchange], policy: RateLimitPolicy)

    // can be executed after DiscoverSecurities is completed
    // can obtain stream of all tickers from security repository and then enrich profiles
    case EnrichCompanyProfiles(policy: RateLimitPolicy)

    // can be executed after EnrichCompanyProfiles is completed
    case FetchPricePerformanceSummaries(filter: CompanyProfileFilter, policy: RateLimitPolicy, limit: Option[Int] = None)

    case FetchLatestSecurities(exchange: Exchange)
    case FetchCompanyProfile(ticker: Ticker)
    case FetchLatestPricePerformanceSummary(ticker: Ticker)

  object Action {
    given JsonTaggedAdt.Config[Action] = JsonTaggedAdt.Config.Values[Action](
      mappings = Map(
        "fetch-latest-securities"                -> JsonTaggedAdt.tagged[Action.FetchLatestSecurities],
        "fetch-company-profile"                  -> JsonTaggedAdt.tagged[Action.FetchCompanyProfile],
        "fetch-latest-price-performance-summary" -> JsonTaggedAdt.tagged[Action.FetchLatestPricePerformanceSummary],
        "schedule"                               -> JsonTaggedAdt.tagged[Action.Schedule],
        "reschedule-all"                         -> JsonTaggedAdt.tagged[Action.RescheduleAll.type]
      ),
      strict = true,
      typeFieldName = "kind"
    )
  }
}
