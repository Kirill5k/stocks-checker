package stockschecker

import cats.data.NonEmptyList
import stockschecker.common.JsonCodecs
import stockschecker.domain.{CommandId, CompanyProfileFilter, Exchange, Ticker}
import org.latestbit.circe.adt.codec.*

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
