package stockschecker.actions

import cats.data.NonEmptyList
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import stockschecker.domain.*

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
    )
  )

  "Action codec" should {
    "correctly encode and decode all action types" in
      forAll(testCases) { (json, action) =>
        withClue(s"Testing ${action.getClass.getSimpleName}: ") {
          action.asJson.noSpaces mustBe json
          decode[Action](json) mustBe Right(action)
        }
      }
  }
}
