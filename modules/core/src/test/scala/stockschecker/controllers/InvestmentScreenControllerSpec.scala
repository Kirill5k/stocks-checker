package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.services.InvestmentScreenService
import java.time.Instant
import io.circe.syntax.*

class InvestmentScreenControllerSpec extends HttpRoutesWordSpec:
  private val config = ApiConfig("screen-test-key")
  private val authHeader = Header.Raw(CIString("X-API-Key"), config.key)

  "Investment screen endpoints" should {
    "require API authentication before creating a screen" in {
      val service = mock[InvestmentScreenService[IO]]
      val result = for
        controller <- InvestmentScreenController.make(service, config)
        response <- controller.routes.orNotFound.run(Request[IO](method = Method.POST, uri = uri"/investment-screens").withEntity("{}"))
      yield response
      result.mustHaveStatus(Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
      verifyNoInteractions(service)
    }
    "route evaluation requests to the saved cohort evaluation" in {
      val service = mock[InvestmentScreenService[IO]]
      val now = Instant.parse("2026-10-19T00:00:00Z")
      val evaluation = InvestmentEvaluator.evaluate("screen-1", now, now, Nil, Map.empty)
      when(service.evaluate("screen-1")).thenReturnIO(evaluation)
      val result = for
        controller <- InvestmentScreenController.make(service, config)
        response <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-screens/screen-1/evaluation").withHeaders(authHeader))
      yield response
      result.mustHaveStatus(Status.Ok, Some(evaluation.asJson.noSpaces))
      verify(service).evaluate("screen-1")
    }
    "apply a bounded default listing size through the service" in {
      val service = mock[InvestmentScreenService[IO]]
      when(service.list(20)).thenReturnIO(Nil)
      val result = for
        controller <- InvestmentScreenController.make(service, config)
        response <- controller.routes.orNotFound.run(Request[IO](uri = uri"/investment-screens").withHeaders(authHeader))
      yield response
      result.mustHaveStatus(Status.Ok, Some("[]"))
      verify(service).list(20)
    }
  }
