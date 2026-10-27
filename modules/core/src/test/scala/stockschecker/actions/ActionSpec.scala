package stockschecker.actions

import cats.data.NonEmptyList
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.*

import java.time.LocalDate
import scala.concurrent.duration.*

class ActionSpec extends AnyWordSpec with Matchers {
  private val actions = List(
    Action.Sequence(NonEmptyList.of(Action.DeactivateSecurity(Ticker("AAPL")), Action.DeactivateCompanyProfile(Ticker("MSFT")))),
    Action.FetchSecurities(NonEmptyList.of(Exchange.NYSE, Exchange.NASDAQ)),
    Action.FetchCompanyProfiles(SecurityFilter.IsActive(true), Some(100)),
    Action.FetchFinancialMetrics(CompanyProfileFilter.IsActive(true)),
    Action.FetchPriceAnalytics(CompanyProfileFilter.CountryIs("US")),
    Action.UpdateCompanyProfiles(NonEmptyList.one(Ticker("AAPL"))),
    Action.UpdateFinancialMetrics(NonEmptyList.one(Ticker("AAPL"))),
    Action.UpdatePriceAnalytics(NonEmptyList.one(Ticker("AAPL"))),
    Action.RefreshInvestmentResearch,
    Action.RefreshEvaluationPrices(Ticker("AAPL"), LocalDate.parse("2025-01-01"), LocalDate.parse("2026-01-01")),
    Action.DeactivateSecurity(Ticker("AAPL")),
    Action.DeactivateCompanyProfile(Ticker("MSFT"))
  )

  "Action codec" should {
    "round-trip every supported action" in
      actions.foreach(action => decode[Action](action.asJson.noSpaces) mustBe Right(action))

    "preserve wire names and optional defaults" in {
      decode[Action]("""{"kind":"fetch-company-profiles","filter":{"kind":"is-active","active":true}}""") mustBe
        Right(Action.FetchCompanyProfiles(SecurityFilter.IsActive(true)))
      decode[Action]("""{"kind":"deactivate-security","ticker":"AAPL","extra":true}""") mustBe
        Right(Action.DeactivateSecurity(Ticker("AAPL")))
      Action.RefreshInvestmentResearch.asJson.noSpaces mustBe """{"kind":"refresh-investment-research"}"""
    }

    "decode supported nested actions from existing commands" in {
      val json   = """{
        "id":"cmd-123","isActive":true,
        "action":{"kind":"sequence","actions":[
          {"kind":"fetch-company-profiles","filter":{"kind":"is-active","active":true}},
          {"kind":"deactivate-security","ticker":"AAPL"}
        ]},
        "schedule":{"kind":"periodic","period":"10seconds"},
        "lastExecutedAt":null,"executionCount":0,"maxExecutions":null,"version":0
      }"""
      val action = Action.Sequence(
        NonEmptyList.of(Action.FetchCompanyProfiles(SecurityFilter.IsActive(true)), Action.DeactivateSecurity(Ticker("AAPL")))
      )
      decode[Command](json) mustBe Right(Command(CommandId("cmd-123"), true, action, Schedule.Periodic(10.seconds), None, 0, None))
    }

    "reject unknown discriminators and malformed supported actions" in
      List(
        """{}""",
        """{"kind":"unknown"}""",
        """{"kind":123}""",
        """{"kind":"sequence","actions":[]}""",
        """{"kind":"deactivate-security"}"""
      )
        .foreach(json => decode[Action](json).isLeft mustBe true)

    "keep job state strings compatible while rejecting invalid states" in {
      JobState.values.foreach(state => decode[JobState](state.asJson.noSpaces) mustBe Right(state))
      Job.Succeeded.asJson.noSpaces mustBe "\"succeeded\""
      decode[JobState]("\"typo\"").isLeft mustBe true
    }
  }
}
