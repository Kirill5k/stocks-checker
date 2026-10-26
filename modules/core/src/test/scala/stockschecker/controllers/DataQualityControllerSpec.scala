package stockschecker.controllers

import cats.effect.IO
import io.circe.syntax.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.{Header, Method, Request, Status}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.DataQualityReport
import stockschecker.services.DataQualityService

import java.time.Instant

class DataQualityControllerSpec extends HttpRoutesWordSpec {
  private val config = ApiConfig("test-key")

  "Data quality endpoint" should {
    "require authentication before reading the report" in {
      val service  = mock[DataQualityService[IO]]
      val response = for
        controller <- DataQualityController.make(service, config)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/data-quality"))
      yield response
      response mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
      verifyNoInteractions(service)
    }

    "return the data quality report after authentication" in {
      val service = mock[DataQualityService[IO]]
      val report  = DataQualityReport(Instant.parse("2026-10-26T12:00:00Z"), Nil, Nil, Map.empty, Map.empty)
      when(service.report).thenReturnIO(report)
      val response = for
        controller <- DataQualityController.make(service, config)
        response   <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/data-quality")
            .withHeaders(Header.Raw(CIString("X-API-Key"), config.key))
        )
      yield response
      response.mustHaveStatus(Status.Ok, Some(report.asJson.noSpaces))
      verify(service).report
    }

    "leave the removed provider resume endpoint unregistered" in {
      val service  = mock[DataQualityService[IO]]
      val response = for
        controller <- DataQualityController.make(service, config)
        response   <- controller.routes.orNotFound.run(
          Request[IO](method = Method.POST, uri = uri"/providers/Finnhub/resume")
            .withHeaders(Header.Raw(CIString("X-API-Key"), config.key))
        )
      yield response
      response mustHaveStatus (Status.NotFound, Some("Not found"))
      verifyNoInteractions(service)
    }
  }
}
