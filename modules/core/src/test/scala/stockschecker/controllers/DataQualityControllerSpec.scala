package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.{Header, Method, Request, Status}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.services.DataQualityService

class DataQualityControllerSpec extends HttpRoutesWordSpec {
  private val config = ApiConfig("test-key")

  "Provider resume endpoint" should {
    "require authentication before resuming provider jobs" in {
      val service  = mock[DataQualityService[IO]]
      val response = for
        controller <- DataQualityController.make(service, config)
        response   <- controller.routes.orNotFound.run(Request[IO](method = Method.POST, uri = uri"/providers/Finnhub/resume"))
      yield response
      response mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
      verifyNoInteractions(service)
    }

    "resume the selected provider after authentication" in {
      val service = mock[DataQualityService[IO]]
      when(service.resumeProvider("Finnhub")).thenReturnUnit
      val response = for
        controller <- DataQualityController.make(service, config)
        response   <- controller.routes.orNotFound.run(
          Request[IO](method = Method.POST, uri = uri"/providers/Finnhub/resume")
            .withHeaders(Header.Raw(CIString("X-API-Key"), config.key))
        )
      yield response
      response mustHaveStatus Status.Ok
      verify(service).resumeProvider("Finnhub")
    }
  }
}
