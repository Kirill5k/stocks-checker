package stockschecker.controllers

import cats.effect.IO
import io.circe.syntax.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{Stock, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.services.StockService
import stockschecker.fixtures.*

class StockControllerSpec extends HttpRoutesWordSpec {

  val testApiKey   = "test-api-key-12345"
  val apiConfig    = ApiConfig(testApiKey)
  val apiKeyHeader = Header.Raw(CIString("X-API-Key"), testApiKey)

  "A StockController" when {
    "GET /stocks/:ticker" should {
      "return 200 and stock on success" in {
        val svc = mocks
        when(svc.findByTicker(any[Ticker])).thenReturnIO(AAPLStock)

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks/aapl", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some(AAPLStock.asJson.noSpaces))
        verify(svc).findByTicker(AAPL)
      }

      "return 404 on not found" in {
        val svc = mocks
        when(svc.findByTicker(any[Ticker])).thenRaiseError(AppError.SecurityNotFound(Ticker("UNKNOWN")))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks/UNKNOWN", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find security for UNKNOWN"}"""))
        verify(svc).findByTicker(Ticker("UNKNOWN"))
      }
    }
  }

  def mocks: StockService[IO] = mock[StockService[IO]]
}