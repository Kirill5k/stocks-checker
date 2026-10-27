package stockschecker.actions

import cats.data.NonEmptyList
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.*

import java.time.{Instant, LocalDate}
import scala.concurrent.duration.*

class ActionSpec extends AnyWordSpec with Matchers {
  private val requestedAt = Instant.parse("2026-10-27T12:00:00Z")
  private val actions     = List(
    Action.Sequence(
      NonEmptyList.of(
        Action.SetSecurityActive(Ticker("AAPL"), false, requestedAt),
        Action.SetCompanyProfileActive(Ticker("MSFT"), false, requestedAt)
      )
    ),
    Action.FetchSecurities(NonEmptyList.of(Exchange.NYSE, Exchange.NASDAQ)),
    Action.FetchCompanyProfiles(SecurityFilter.IsActive(true), Some(100)),
    Action.FetchFinancialMetrics(CompanyProfileFilter.IsActive(true)),
    Action.FetchPriceAnalytics(CompanyProfileFilter.CountryIs("US")),
    Action.UpdateCompanyProfiles(NonEmptyList.one(Ticker("AAPL"))),
    Action.UpdateFinancialMetrics(NonEmptyList.one(Ticker("AAPL"))),
    Action.UpdatePriceAnalytics(NonEmptyList.one(Ticker("AAPL"))),
    Action.RefreshInvestmentResearch,
    Action.RefreshEvaluationPrices(Ticker("AAPL"), LocalDate.parse("2026-01-01")),
    Action.RecordCompanyProfileUpdate(NonEmptyList.of(Ticker("AAPL"), Ticker("MSFT"))),
    Action.RecordPriceAnalyticsUpdate(NonEmptyList.one(Ticker("AAPL"))),
    Action.RecordFinancialMetricsUpdate(NonEmptyList.one(Ticker("AAPL"))),
    Action.SetSecurityActive(Ticker("AAPL"), true, requestedAt),
    Action.SetCompanyProfileActive(Ticker("AAPL"), true, requestedAt),
    Action.SetSecurityActive(Ticker("AAPL"), false, requestedAt),
    Action.SetCompanyProfileActive(Ticker("MSFT"), false, requestedAt)
  )

  "Action codec" should {
    "round-trip every supported action" in
      actions.foreach(action => decode[Action](action.asJson.noSpaces) mustBe Right(action))

    "preserve wire names and optional defaults" in {
      decode[Action]("""{"kind":"fetch-company-profiles","filter":{"kind":"is-active","active":true}}""") mustBe
        Right(Action.FetchCompanyProfiles(SecurityFilter.IsActive(true)))
      decode[Action](
        """{"kind":"set-security-active","ticker":"AAPL","active":false,"requestedAt":"2026-10-27T12:00:00Z","extra":true}"""
      ) mustBe
        Right(Action.SetSecurityActive(Ticker("AAPL"), false, requestedAt))
      Action.RefreshInvestmentResearch.asJson.noSpaces mustBe """{"kind":"refresh-investment-research"}"""
    }

    "decode supported nested actions from existing commands" in {
      val json   = """{
        "id":"cmd-123","isActive":true,
        "action":{"kind":"sequence","actions":[
          {"kind":"fetch-company-profiles","filter":{"kind":"is-active","active":true}},
          {"kind":"set-security-active","ticker":"AAPL","active":false,"requestedAt":"2026-10-27T12:00:00Z"}
        ]},
        "schedule":{"kind":"periodic","period":"10seconds"},
        "lastExecutedAt":null,"executionCount":0,"maxExecutions":null,"version":0
      }"""
      val action = Action.Sequence(
        NonEmptyList.of(
          Action.FetchCompanyProfiles(SecurityFilter.IsActive(true)),
          Action.SetSecurityActive(Ticker("AAPL"), false, requestedAt)
        )
      )
      decode[Command](json) mustBe Right(Command(CommandId("cmd-123"), true, action, Schedule.Periodic(10.seconds), None, 0, None))
    }

    "reject unknown discriminators and malformed supported actions" in
      List(
        """{}""",
        """{"kind":"unknown"}""",
        """{"kind":123}""",
        """{"kind":"sequence","actions":[]}""",
        """{"kind":"set-security-active"}""",
        """{"kind":"set-security-active","ticker":"AAPL","active":true}"""
      )
        .foreach(json => decode[Action](json).isLeft mustBe true)

    "keep job state strings compatible while rejecting invalid states" in {
      JobState.values.foreach(state => decode[JobState](state.asJson.noSpaces) mustBe Right(state))
      Job.Succeeded.asJson.noSpaces mustBe "\"succeeded\""
      decode[JobState]("\"typo\"").isLeft mustBe true
    }
  }

  "Action priority" should {
    "claim internal events before fan-out work and retain lower evaluation priority" in {
      actions.filterNot(_.isSchedulable).filterNot(_.isInstanceOf[Action.RefreshEvaluationPrices]).foreach { action =>
        ActionPriority.forAction(action) mustBe ActionPriority.Internal
      }
      actions.filter(_.isSchedulable).foreach { action =>
        ActionPriority.forAction(action) mustBe ActionPriority.Normal
      }
      ActionPriority.forAction(Action.RefreshEvaluationPrices(Ticker("AAPL"), LocalDate.parse("2026-01-01"))) mustBe
        ActionPriority.Evaluation
      ActionPriority.Internal must be < ActionPriority.Normal
      ActionPriority.Normal must be < ActionPriority.Evaluation
    }
  }

  "Action scheduling" should {
    "accept public operations and recursively reject internal actions" in {
      val publicActions = actions.collect {
        case action: Action.FetchSecurities        => action
        case action: Action.FetchCompanyProfiles   => action
        case action: Action.FetchFinancialMetrics  => action
        case action: Action.FetchPriceAnalytics    => action
        case action: Action.UpdateCompanyProfiles  => action
        case action: Action.UpdateFinancialMetrics => action
        case action: Action.UpdatePriceAnalytics   => action
        case Action.RefreshInvestmentResearch      => Action.RefreshInvestmentResearch
      }
      publicActions.foreach(_.isSchedulable mustBe true)
      actions.filterNot(publicActions.contains).foreach(_.isSchedulable mustBe false)
      Action.Sequence(NonEmptyList.fromListUnsafe(publicActions)).isSchedulable mustBe true
      Action
        .Sequence(
          NonEmptyList.of(
            Action.RefreshInvestmentResearch,
            Action.Sequence(NonEmptyList.one(Action.RecordPriceAnalyticsUpdate(NonEmptyList.one(Ticker("AAPL")))))
          )
        )
        .isSchedulable mustBe false
    }
  }
}
