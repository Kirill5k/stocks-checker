package stockschecker.actions

import cats.data.NonEmptyList
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.*

import scala.concurrent.duration.*

class ActionSpec extends AnyWordSpec with Matchers {
  "Action" should {
    "decode and encode RescheduleAll" in {
      val json = """{"kind":"reschedule-all"}"""
      val action = Action.RescheduleAll

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode Schedule" in {
      val json = """{"cid":"cmd-123","duration":"10seconds","kind":"schedule"}"""
      val action = Action.Schedule(CommandId("cmd-123"), 10.seconds)

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode Sequence" in {
      val json = """{"actions":[{"kind":"reschedule-all"}],"kind":"sequence"}"""
      val action = Action.Sequence(NonEmptyList.one(Action.RescheduleAll))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode FetchSecurities" in {
      val json = """{"exchanges":["nyse","nasdaq"],"kind":"fetch-securities"}"""
      val action = Action.FetchSecurities(NonEmptyList.of(Exchange.NYSE, Exchange.NASDAQ))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode FetchCompanyProfiles" in {
      val json = """{"filter":{"exchange":"nasdaq","kind":"exchange-is"},"limit":100,"kind":"fetch-company-profiles"}"""
      val action = Action.FetchCompanyProfiles(SecurityFilter.ExchangeIs(Exchange.NASDAQ), Some(100))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode FetchCompanyProfiles without limit" in {
      val json = """{"filter":{"active":true,"kind":"is-active"},"limit":null,"kind":"fetch-company-profiles"}"""
      val action = Action.FetchCompanyProfiles(SecurityFilter.IsActive(true), None)

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode FetchFinancialMetrics" in {
      val json = """{"filter":{"min":1000000000,"kind":"market-cap-above"},"limit":50,"kind":"fetch-financial-metrics"}"""
      val action = Action.FetchFinancialMetrics(CompanyProfileFilter.MarketCapAbove(1000000000L), Some(50))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode FetchPriceAnalytics" in {
      val json = """{"filter":{"countryCode":"US","kind":"country-is"},"limit":null,"kind":"fetch-price-analytics"}"""
      val action = Action.FetchPriceAnalytics(CompanyProfileFilter.CountryIs("US"), None)

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode UpdateCompanyProfiles" in {
      val json = """{"tickers":["AAPL","MSFT"],"kind":"update-company-profiles"}"""
      val action = Action.UpdateCompanyProfiles(NonEmptyList.of(Ticker("AAPL"), Ticker("MSFT")))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode UpdateFinancialMetrics" in {
      val json = """{"tickers":["GOOGL"],"kind":"update-financial-metrics"}"""
      val action = Action.UpdateFinancialMetrics(NonEmptyList.one(Ticker("GOOGL")))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode UpdatePriceAnalytics" in {
      val json = """{"tickers":["TSLA","NVDA"],"kind":"update-price-analytics"}"""
      val action = Action.UpdatePriceAnalytics(NonEmptyList.of(Ticker("TSLA"), Ticker("NVDA")))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode RecordCompanyProfileUpdate with empty list" in {
      val json = """{"tickers":[],"kind":"record-company-profile-update"}"""
      val action = Action.RecordCompanyProfileUpdate(List.empty)

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode RecordCompanyProfileUpdate with tickers" in {
      val json = """{"tickers":["AAPL","MSFT"],"kind":"record-company-profile-update"}"""
      val action = Action.RecordCompanyProfileUpdate(List(Ticker("AAPL"), Ticker("MSFT")))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode RecordFinancialMetricsUpdate" in {
      val json = """{"tickers":["AMZN"],"kind":"record-financial-metrics-update"}"""
      val action = Action.RecordFinancialMetricsUpdate(List(Ticker("AMZN")))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode RecordPriceAnalyticsUpdate" in {
      val json = """{"tickers":["META","NFLX"],"kind":"record-price-analytics-update"}"""
      val action = Action.RecordPriceAnalyticsUpdate(List(Ticker("META"), Ticker("NFLX")))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }

    "decode and encode complex nested Sequence" in {
      val json = """{"actions":[{"exchanges":["nyse"],"kind":"fetch-securities"},{"tickers":["AAPL"],"kind":"update-company-profiles"}],"kind":"sequence"}"""
      val action = Action.Sequence(NonEmptyList.of(
        Action.FetchSecurities(NonEmptyList.one(Exchange.NYSE)),
        Action.UpdateCompanyProfiles(NonEmptyList.one(Ticker("AAPL")))
      ))

      action.asJson.noSpaces mustBe json
      decode[Action](json) mustBe Right(action)
    }
  }
}
