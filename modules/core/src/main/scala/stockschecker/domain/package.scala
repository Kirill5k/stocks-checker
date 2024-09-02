package stockschecker

import io.circe.Codec as CirceCodec
import stockschecker.common.types.StringType
import sttp.tapir.{Codec, DecodeResult, Schema}

import java.time.{Instant, LocalDate}

package object domain {

  opaque type Ticker = String
  object Ticker extends StringType[Ticker] {
    inline given Codec.PlainCodec[Ticker] = Codec.string.mapDecode[Ticker](s => DecodeResult.Value(Ticker(s)))(_.value)
    given Schema[Ticker] = Schema.string
  }

  final case class Stock(
      ticker: Ticker,
      price: BigDecimal,
      stockType: String,
      lastUpdatedAt: Instant,
      priceDelta: Option[BigDecimal] = None
  ) derives CirceCodec.AsObject

  final case class CompanyProfile(
      ticker: Ticker,
      name: String,
      country: String,
      sector: String,
      industry: String,
      description: String,
      website: String,
      ipoDate: LocalDate,
      currency: String,
      marketCap: Long,
      averageTradedVolume: Long,
      isEtf: Boolean,
      isActivelyTrading: Boolean,
      isFund: Boolean,
      isAdr: Boolean,
      lastUpdatedAt: Instant
  ) derives CirceCodec.AsObject
}
