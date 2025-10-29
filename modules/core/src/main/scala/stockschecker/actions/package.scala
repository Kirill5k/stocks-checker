package stockschecker

import cats.data.NonEmptyList
import stockschecker.common.JsonCodecs
import stockschecker.domain.{CommandId, Exchange, Ticker}
import org.latestbit.circe.adt.codec.*

import scala.concurrent.duration.FiniteDuration

package object actions extends JsonCodecs {

  enum Action derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
    case RescheduleAll
    case Schedule(cid: CommandId, duration: FiniteDuration)
    case FetchLatestSecurities(exchanges: NonEmptyList[Exchange])
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
