package stockschecker

import cats.data.NonEmptyList
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json, JsonObject}
import io.circe.syntax.given
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
    case FetchMonthlyStockData(ticker: Ticker)

  object Action {
    given JsonTaggedAdt.Config[Action] = JsonTaggedAdt.Config.Values[Action](
      mappings = Map(
        "fetch-latest-securities"  -> JsonTaggedAdt.tagged[Action.FetchLatestSecurities],
        "fetch-company-profile"    -> JsonTaggedAdt.tagged[Action.FetchCompanyProfile],
        "fetch-monthly-stock-data" -> JsonTaggedAdt.tagged[Action.FetchMonthlyStockData],
        "schedule"                 -> JsonTaggedAdt.tagged[Action.Schedule],
        "reschedule-all"           -> JsonTaggedAdt.tagged[Action.RescheduleAll.type]
      ),
      strict = true,
      typeFieldName = "kind"
    )
  }
}
