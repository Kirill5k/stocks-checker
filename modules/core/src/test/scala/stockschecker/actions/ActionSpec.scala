package stockschecker.actions

import cats.data.NonEmptyList
import io.circe.parser.{decode, parse}
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import stockschecker.domain.*

import java.time.LocalDate
import scala.concurrent.duration.*

class ActionSpec extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {

  private val testCases = Table(
    ("json", "action"),
    ("""{"kind":"reschedule-all"}""", Action.RescheduleAll),
    ("""{"cid":"cmd-123","duration":"10seconds","kind":"schedule"}""", Action.Schedule(CommandId("cmd-123"), 10.seconds)),
    ("""{"actions":[{"kind":"reschedule-all"}],"kind":"sequence"}""", Action.Sequence(NonEmptyList.one(Action.RescheduleAll))),
    (
      """{"exchanges":["nyse","nasdaq"],"kind":"fetch-securities"}""",
      Action.FetchSecurities(NonEmptyList.of(Exchange.NYSE, Exchange.NASDAQ))
    ),
    (
      """{"filter":{"exchange":"nasdaq","kind":"exchange-is"},"limit":100,"kind":"fetch-company-profiles"}""",
      Action.FetchCompanyProfiles(SecurityFilter.ExchangeIs(Exchange.NASDAQ), Some(100))
    ),
    (
      """{"filter":{"active":true,"kind":"is-active"},"limit":null,"kind":"fetch-company-profiles"}""",
      Action.FetchCompanyProfiles(SecurityFilter.IsActive(true), None)
    ),
    (
      """{"filter":{"min":1000000000,"kind":"market-cap-above"},"limit":50,"kind":"fetch-financial-metrics"}""",
      Action.FetchFinancialMetrics(CompanyProfileFilter.MarketCapAbove(1000000000L), Some(50))
    ),
    (
      """{"filter":{"countryCode":"US","kind":"country-is"},"limit":null,"kind":"fetch-price-analytics"}""",
      Action.FetchPriceAnalytics(CompanyProfileFilter.CountryIs("US"), None)
    ),
    (
      """{"tickers":["AAPL","MSFT"],"kind":"update-company-profiles"}""",
      Action.UpdateCompanyProfiles(NonEmptyList.of(Ticker("AAPL"), Ticker("MSFT")))
    ),
    ("""{"tickers":["GOOGL"],"kind":"update-financial-metrics"}""", Action.UpdateFinancialMetrics(NonEmptyList.one(Ticker("GOOGL")))),
    (
      """{"ticker":"AAPL","from":"2025-01-01","through":"2026-01-01","kind":"refresh-evaluation-prices"}""",
      Action.RefreshEvaluationPrices(Ticker("AAPL"), LocalDate.parse("2025-01-01"), LocalDate.parse("2026-01-01"))
    ),
    (
      """{"tickers":["TSLA","NVDA"],"kind":"update-price-analytics"}""",
      Action.UpdatePriceAnalytics(NonEmptyList.of(Ticker("TSLA"), Ticker("NVDA")))
    ),
    ("""{"tickers":[],"kind":"record-company-profile-update"}""", Action.RecordCompanyProfileUpdate(List.empty)),
    (
      """{"tickers":["AAPL","MSFT"],"kind":"record-company-profile-update"}""",
      Action.RecordCompanyProfileUpdate(List(Ticker("AAPL"), Ticker("MSFT")))
    ),
    ("""{"tickers":["AMZN"],"kind":"record-financial-metrics-update"}""", Action.RecordFinancialMetricsUpdate(List(Ticker("AMZN")))),
    (
      """{"tickers":["META","NFLX"],"kind":"record-price-analytics-update"}""",
      Action.RecordPriceAnalyticsUpdate(List(Ticker("META"), Ticker("NFLX")))
    ),
    (
      """{"actions":[{"exchanges":["nyse"],"kind":"fetch-securities"},{"tickers":["AAPL"],"kind":"update-company-profiles"}],"kind":"sequence"}""",
      Action.Sequence(
        NonEmptyList.of(
          Action.FetchSecurities(NonEmptyList.one(Exchange.NYSE)),
          Action.UpdateCompanyProfiles(NonEmptyList.one(Ticker("AAPL")))
        )
      )
    ),
    (
      """{"action":{"kind":"reschedule-all"},"attempt":1,"kind":"retried"}""",
      Action.Retried(Action.RescheduleAll, 1)
    ),
    ("""{"ticker":"AAPL","kind":"deactivate-security"}""", Action.DeactivateSecurity(Ticker("AAPL"))),
    ("""{"ticker":"MSFT","kind":"deactivate-company-profile"}""", Action.DeactivateCompanyProfile(Ticker("MSFT")))
  )

  "Action codec" should {
    "correctly encode and decode all action types" in
      forAll(testCases) { (json, action) =>
        withClue(s"Testing ${action.getClass.getSimpleName}: ") {
          Right(action.asJson) mustBe parse(json)
          decode[Action](json) mustBe Right(action)
        }
      }

    "decode missing optional limits in existing payloads" in {
      val filter = """{"kind":"is-active","active":true}"""
      val cases  = Table(
        ("kind", "action"),
        ("fetch-company-profiles", Action.FetchCompanyProfiles(SecurityFilter.IsActive(true))),
        ("fetch-financial-metrics", Action.FetchFinancialMetrics(CompanyProfileFilter.IsActive(true))),
        ("fetch-price-analytics", Action.FetchPriceAnalytics(CompanyProfileFilter.IsActive(true)))
      )

      forAll(cases) { (kind, action) =>
        decode[Action](s"""{"kind":"$kind","filter":$filter}""") mustBe Right(action)
      }
    }

    "continue accepting extra fields in existing payloads" in {
      decode[Action]("""{"kind":"reschedule-all","extra":true}""") mustBe Right(Action.RescheduleAll)
      decode[Action]("""{"kind":"deactivate-security","ticker":"AAPL","extra":true}""") mustBe
        Right(Action.DeactivateSecurity(Ticker("AAPL")))
    }

    "reject invalid discriminators and malformed action fields" in {
      val invalid = Table(
        "json",
        """{}""",
        """{"kind":"unknown"}""",
        """{"kind":"RescheduleAll"}""",
        """{"kind":null}""",
        """{"kind":123}""",
        """{"kind":"schedule","cid":"cmd-123"}""",
        """{"kind":"retried","action":{"kind":"reschedule-all"}}""",
        """{"kind":"sequence","actions":[]}""",
        """{"kind":"sequence","actions":[{"kind":"unknown"}]}""",
        """{"kind":"fetch-company-profiles","filter":{"kind":"is-active","active":true},"limit":"bad"}"""
      )

      forAll(invalid) { json =>
        withClue(json) {
          decode[Action](json).isLeft mustBe true
        }
      }
    }

    "decode stored commands containing nested actions" in {
      val json   = """{
        "id":"cmd-123",
        "isActive":true,
        "action":{"kind":"sequence","actions":[
          {"kind":"fetch-company-profiles","filter":{"kind":"composite","filters":[
            {"kind":"exchange-is","exchange":"nasdaq"},{"kind":"is-active","active":true}
          ]}},
          {"kind":"retried","action":{"kind":"reschedule-all"},"attempt":2}
        ]},
        "schedule":{"kind":"periodic","period":"10seconds"},
        "lastExecutedAt":null,
        "executionCount":0,
        "maxExecutions":null
      }"""
      val action = Action.Sequence(
        NonEmptyList.of(
          Action.FetchCompanyProfiles(
            SecurityFilter.Composite(
              NonEmptyList.of(
                SecurityFilter.ExchangeIs(Exchange.NASDAQ),
                SecurityFilter.IsActive(true)
              )
            )
          ),
          Action.Retried(Action.RescheduleAll, 2)
        )
      )

      decode[Command](json) mustBe Right(Command(CommandId("cmd-123"), true, action, Schedule.Periodic(10.seconds), None, 0, None))
    }
  }
}
