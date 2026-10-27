package stockschecker.domain

import io.circe.{Decoder, Encoder, Json}
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.YearMonth

class ResearchCodecsSpec extends AnyWordSpec with Matchers {
  private def roundTrip[A: Encoder: Decoder](value: A, json: String): org.scalatest.Assertion = {
    value.asJson.noSpaces mustBe json
    decode[A](json) mustBe Right(value)
  }

  "Research JSON" should {
    "retain provider names, state strings, cohort names and month format" in {
      roundTrip[Provider](Provider.Finnhub, "\"Finnhub\"")
      roundTrip[Provider](Provider.TwelveData, "\"TwelveData\"")
      roundTrip[ListingStatus](ListingStatus.Delisted, "\"delisted\"")
      roundTrip[DatasetStatus](DatasetStatus.ConfigurationError, "\"configuration-error\"")
      roundTrip[Dataset](Dataset.CompanyProfile, "\"profile\"")
      roundTrip[Dataset](Dataset.Financials, "\"financials\"")
      roundTrip[Dataset](Dataset.DailyPricesUnadjusted, "\"daily-prices-none\"")
      roundTrip[Dataset](Dataset.DailyPricesSplits, "\"daily-prices-splits\"")
      roundTrip[Dataset](Dataset.DailyPricesSplitsAndDividends, "\"daily-prices-all\"")
      roundTrip[EvaluationStatus](EvaluationStatus.Incomplete, "\"incomplete\"")
      roundTrip[CohortName](CohortName.OldPriceScore, "\"old-price-score\"")
      roundTrip[CohortName](CohortName.Vti, "\"vti\"")
      roundTrip[PriceAdjustment](PriceAdjustment.SplitsAndDividends, "\"split-and-dividend\"")
      roundTrip[YearMonth](YearMonth.of(2026, 10), "\"2026-10\"")
    }

    "encode typed map keys as the existing object keys" in {
      roundTrip[Map[Pillar, BigDecimal]](Map(Pillar.Quality -> BigDecimal(75)), """{"quality":75}""")
      roundTrip[Map[DatasetStatus, Int]](Map(DatasetStatus.Available -> 2), """{"available":2}""")
    }

    "reject unknown states and pillar keys" in {
      decode[EvaluationStatus](Json.fromString("finished").noSpaces).isLeft mustBe true
      decode[Map[Pillar, BigDecimal]]("""{"unknown":75}""").isLeft mustBe true
    }
  }
}
