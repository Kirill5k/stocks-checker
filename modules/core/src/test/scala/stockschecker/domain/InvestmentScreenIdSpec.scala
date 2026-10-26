package stockschecker.domain

import io.circe.Json
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sttp.tapir.{Codec, DecodeResult}

class InvestmentScreenIdSpec extends AnyWordSpec with Matchers {
  private val values = List("7a6f7470-6131-4b64-bd94-e822e3f504c9", "monthly:2026-10:quality-fair-value-v1")

  "InvestmentScreenId" should {
    "retain the existing string JSON format for user and monthly screens" in
      values.foreach { value =>
        val id = InvestmentScreenId(value)
        id.asJson mustBe Json.fromString(value)
        Json.fromString(value).as[InvestmentScreenId] mustBe Right(id)
      }

    "preserve screen IDs when decoding HTTP paths" in {
      val codec = summon[Codec.PlainCodec[InvestmentScreenId]]
      values.foreach { value =>
        codec.decode(value) mustBe DecodeResult.Value(InvestmentScreenId(value))
        codec.encode(InvestmentScreenId(value)) mustBe value
      }
    }

    "require explicit construction instead of accepting strings or tickers" in {
      assertDoesNotCompile("""val id: stockschecker.domain.InvestmentScreenId = "screen-1"""")
      assertDoesNotCompile("""val id: stockschecker.domain.InvestmentScreenId = stockschecker.domain.Ticker("AAPL")""")
    }
  }
}
