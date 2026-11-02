package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.services.{InvestmentEvaluationService, InvestmentScreenService}
import java.time.Instant
import io.circe.syntax.*

class InvestmentScreenControllerSpec extends HttpRoutesWordSpec:
  private val config     = ApiConfig("screen-test-key")
  private val authHeader = Header.Raw(CIString("X-API-Key"), config.key)

  "Investment screen endpoints" should {
    "route evaluation requests to the saved cohort evaluation" in {
      val service    = mock[InvestmentScreenService[IO]]
      val now        = Instant.parse("2026-10-19T00:00:00Z")
      val id         = InvestmentScreenId("screen-1")
      val evaluation = InvestmentEvaluator.evaluate(
        id,
        now,
        now,
        Nil,
        Map.empty,
        java.time.YearMonth.of(2026, 10),
        java.time.LocalDate.parse("2026-10-01")
      )
      val evaluations = mock[InvestmentEvaluationService[IO]]
      when(evaluations.evaluate(id)).thenReturnIO(evaluation)
      val result = for
        controller <- InvestmentScreenController.make(service, evaluations, config)
        response   <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/investment-screens/screen-1/evaluation").withHeaders(authHeader)
        )
      yield response
      result.mustHaveStatus(Status.Ok, Some(evaluation.asJson.noSpaces))
      verify(evaluations).evaluate(id)
    }
    "preserve monthly screen IDs in paths and JSON responses" in {
      val service = mock[InvestmentScreenService[IO]]
      val id      = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      val summary = InvestmentScreenSummary(id, Instant.parse("2026-10-19T00:00:00Z"), "v1", 0, 0, 0, Map.empty)
      val screen  = InvestmentScreenResult(summary, InvestmentScreenRequest(), Nil, Nil, Nil, Nil, InvestmentRankingId("ranking-1"))
      when(service.get(id, false)).thenReturnIO(screen)
      val result = for
        controller <- InvestmentScreenController.make(service, mock[InvestmentEvaluationService[IO]], config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-screens" / id.value).withHeaders(authHeader))
      yield response
      result.mustHaveStatus(Status.Ok, Some(screen.asJson.mapObject(_.remove("exclusions")).noSpaces))
      verify(service).get(id, false)
    }
    "include full exclusions only when explicitly requested" in {
      val service  = mock[InvestmentScreenService[IO]]
      val id       = InvestmentScreenId("screen-1")
      val summary  = InvestmentScreenSummary(id, Instant.parse("2026-10-19T00:00:00Z"), "v1", 1, 0, 0, Map("missing-price" -> 1))
      val excluded = InvestmentScorecard(
        Ticker("AAPL"),
        false,
        None,
        Map.empty,
        Map.empty,
        List("missing-price"),
        Nil,
        None,
        0,
        "v1"
      )
      val screen =
        InvestmentScreenResult(summary, InvestmentScreenRequest(), Nil, Nil, List(excluded), Nil, InvestmentRankingId("ranking-1"))
      when(service.get(id, true)).thenReturnIO(screen)
      val result = for
        controller <- InvestmentScreenController.make(service, mock[InvestmentEvaluationService[IO]], config)
        response   <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/investment-screens/screen-1?includeExclusions=true").withHeaders(authHeader)
        )
      yield response
      result.mustHaveStatus(Status.Ok, Some(screen.asJson.noSpaces))
      verify(service).get(id, true)
    }
    "omit exclusions without changing the saved request or summary counts" in {
      val service  = mock[InvestmentScreenService[IO]]
      val id       = InvestmentScreenId("screen-1")
      val summary  = InvestmentScreenSummary(id, Instant.parse("2026-10-19T00:00:00Z"), "v1", 1, 0, 0, Map("missing-price" -> 1))
      val excluded = InvestmentScorecard(
        Ticker("AAPL"),
        false,
        None,
        Map.empty,
        Map.empty,
        List("missing-price"),
        Nil,
        None,
        0,
        "v1"
      )
      val screen =
        InvestmentScreenResult(summary, InvestmentScreenRequest(), Nil, Nil, List(excluded), Nil, InvestmentRankingId("ranking-1"))
      when(service.get(id, false)).thenReturnIO(screen)
      val result = for
        controller <- InvestmentScreenController.make(service, mock[InvestmentEvaluationService[IO]], config)
        response   <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/investment-screens/screen-1?includeExclusions=false&limit=20").withHeaders(authHeader)
        )
      yield response
      result.mustHaveStatus(Status.Ok, Some(screen.asJson.mapObject(_.remove("exclusions")).noSpaces))
      verify(service).get(id, false)
    }
    "reject an invalid exclusions flag at the HTTP boundary" in {
      val service = mock[InvestmentScreenService[IO]]
      val result  = for
        controller <- InvestmentScreenController.make(service, mock[InvestmentEvaluationService[IO]], config)
        response   <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/investment-screens/screen-1?includeExclusions=not-a-boolean").withHeaders(authHeader)
        )
      yield response
      result.mustHaveStatus(
        Status.UnprocessableContent,
        Some("""{"message":"Invalid value for: query parameter includeExclusions"}""")
      )
      verifyNoInteractions(service)
    }
    "apply a bounded default listing size through the service" in {
      val service = mock[InvestmentScreenService[IO]]
      when(service.list(20)).thenReturnIO(Nil)
      val result = for
        controller <- InvestmentScreenController.make(service, mock[InvestmentEvaluationService[IO]], config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-screens").withHeaders(authHeader))
      yield response
      result.mustHaveStatus(Status.Ok, Some("[]"))
      verify(service).list(20)
    }
  }
