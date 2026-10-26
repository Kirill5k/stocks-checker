package stockschecker.domain

import stockschecker.common.types.StringType
import sttp.tapir.{Codec, DecodeResult, Schema}

opaque type InvestmentScreenId = String
object InvestmentScreenId extends StringType[InvestmentScreenId] {
  inline given Codec.PlainCodec[InvestmentScreenId] =
    Codec.string.mapDecode[InvestmentScreenId](s => DecodeResult.Value(InvestmentScreenId(s)))(_.value)
  given Schema[InvestmentScreenId] = Schema.string
}
