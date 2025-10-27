package stockschecker

import io.circe.Codec as CirceCodec
import org.latestbit.circe.adt.codec.*
import stockschecker.common.types.{EnumType, StringType}
import sttp.tapir.{Codec, DecodeResult, Schema}

import java.time.LocalDate

package object domain {

  opaque type Ticker = String
  object Ticker extends StringType[Ticker] {
    inline given Codec.PlainCodec[Ticker] = Codec.string.mapDecode[Ticker](s => DecodeResult.Value(Ticker(s)))(_.value)
    given Schema[Ticker]                  = Schema.string
  }

  enum Exchange(val code: String, val fullName: String) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
    case NASDAQ extends Exchange("NSQ", "NASDAQ Stock Exchange")
    case NYSE   extends Exchange("NYS", "New York Stock Exchange")

  object Exchange {
    given JsonTaggedAdt.Config[Exchange] = JsonTaggedAdt.Config.Values[Exchange](
      mappings = Map(
        "NSQ" -> JsonTaggedAdt.tagged[Exchange.NASDAQ.type],
        "NYS" -> JsonTaggedAdt.tagged[Exchange.NYSE.type]
      ),
      strict = true,
      typeFieldName = "code"
    )
  }

  object SecurityKind extends EnumType[SecurityKind](() => SecurityKind.values)
  enum SecurityKind:
    case Stock
    case ETF
    case MutualFund
    case REIT
    case ADR

  final case class Security(
      ticker: Ticker,
      exchange: Exchange,
      name: String,
      kind: SecurityKind,
      isActive: Boolean
  ) derives CirceCodec.AsObject

  final case class CompanyProfile(
      name: String,
      country: String,
      sector: String,
      industry: String,
      description: String,
      website: String,
      ipoDate: LocalDate
  ) derives CirceCodec.AsObject
}
