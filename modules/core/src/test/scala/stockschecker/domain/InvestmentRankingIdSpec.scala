package stockschecker.domain

import io.circe.Json
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.UUID

class InvestmentRankingIdSpec extends AnyWordSpec with Matchers {
  "InvestmentRankingId" should {
    "retain the existing string JSON format for generated and legacy ranking IDs" in {
      val generated = InvestmentRankingId.gen
      UUID.fromString(generated.value).toString mustBe generated.value
      List(generated, InvestmentRankingId("published-ranking")).foreach { id =>
        id.asJson mustBe Json.fromString(id.value)
        Json.fromString(id.value).as[InvestmentRankingId] mustBe Right(id)
      }
    }

    "require explicit construction instead of accepting strings or screen IDs" in {
      assertDoesNotCompile("""val id: stockschecker.domain.InvestmentRankingId = "ranking-1"""")
      assertDoesNotCompile(
        """val id: stockschecker.domain.InvestmentRankingId = stockschecker.domain.InvestmentScreenId("screen-1")"""
      )
    }
  }
}
