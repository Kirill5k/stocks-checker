package stockschecker.actions

import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json, JsonObject}
import io.circe.syntax.given
import stockschecker.common.json.given
import stockschecker.domain.CommandId

import scala.concurrent.duration.FiniteDuration

//object Action extends EnumType[Action](() => Action.values)
//enum Action:
//  case FetchLatestStocks
//  case Schedule(cid: CommandId, duration: FiniteDuration)

sealed trait Action(val kind: String)

object Action {
  case object FetchLatestStocks                                      extends Action("fetch-latest-stocks")
  final case class Schedule(cid: CommandId, waiting: FiniteDuration) extends Action("schedule") derives Codec.AsObject

  inline given Decoder[Action] = Decoder.instance { c =>
    c.downField("kind").as[String].flatMap {
      case "fetch-latest-stocks" => Right(FetchLatestStocks)
      case "schedule"            => c.as[Schedule]
      case kind                  => Left(DecodingFailure(s"Unexpected action kind $kind", List(CursorOp.Field("kind"))))
    }
  }

  inline given Encoder[Action] = Encoder.instance {
    case FetchLatestStocks => JsonObject.apply("kind" := FetchLatestStocks.kind).toJson
    case s: Schedule       => s.asJsonObject.add("kind", Json.fromString(s.kind)).asJson
  }
}
