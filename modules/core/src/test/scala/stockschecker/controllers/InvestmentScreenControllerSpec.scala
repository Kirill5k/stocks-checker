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
    "require API authentication before creating a screen" in {
      val service = mock[InvestmentScreenService[IO]]
      val result  = for
        controller <- InvestmentScreenController.make(service, mock[InvestmentEvaluationService[IO]], config)
        response   <- controller.routes.orNotFound.run(Request[IO](method = Method.POST, uri = uri"/investment-screens").withEntity("{}"))
      yield response
      result.mustHaveStatus(Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
      verifyNoInteractions(service)
    }
    "route evaluation requests to the saved cohort evaluation" in {
      val service     = mock[InvestmentScreenService[IO]]
      val now         = Instant.parse("2026-10-19T00:00:00Z")
      val id          = InvestmentScreenId("screen-1")
      val evaluation  = InvestmentEvaluator.evaluate(id, now, now, Nil, Map.empty)
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
      when(service.get(id)).thenReturnIO(screen)
      val result = for
        controller <- InvestmentScreenController.make(service, mock[InvestmentEvaluationService[IO]], config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-screens" / id.value).withHeaders(authHeader))
      yield response
      result.mustHaveStatus(Status.Ok, Some(screen.asJson.noSpaces))
      verify(service).get(id)
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
