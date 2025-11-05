package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import stockschecker.domain.Ticker
import stockschecker.domain.errors.AppError
import stockschecker.services.PriceService
import stockschecker.fixtures.*

class PriceControllerSpec extends HttpRoutesWordSpec {

  "A PriceController" when {
    "GET /price/performance-summary/:ticker" should {
      "return 200 and price performance summary on success" in {
        val svc = mocks
        when(svc.findPerformanceSummary(any[Ticker], anyBoolean)).thenReturnIO(AAPLPricePerformanceSummary)

        val res = for
          controller <- PriceController.make(svc)
          req = Request[IO](uri = uri"/price/performance-summary/AAPL?fetchLatest=true", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = s"""{
                              |  "ticker" : "AAPL",
                              |  "latestPrice" : 228.50,
                              |  "latestPriceDate" : "2025-10-01",
                              |  "oneMonthChange" : 2.47,
                              |  "threeMonthChange" : null,
                              |  "sixMonthChange" : null,
                              |  "oneYearChange" : null,
                              |  "threeYearChange" : null,
                              |  "fiveYearChange" : null,
                              |  "tenYearChange" : null,
                              |  "maxChange" : 4.82
                              |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).findPerformanceSummary(AAPL, true)
      }

      "return 200 and price performance summary when fetchLatest is not provided" in {
        val svc = mocks
        when(svc.findPerformanceSummary(any[Ticker], anyBoolean)).thenReturnIO(AAPLPricePerformanceSummary)

        val res = for
          controller <- PriceController.make(svc)
          req = Request[IO](uri = uri"/price/performance-summary/AAPL", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = s"""{
                              |  "ticker" : "AAPL",
                              |  "latestPrice" : 228.50,
                              |  "latestPriceDate" : "2025-10-01",
                              |  "oneMonthChange" : 2.47,
                              |  "threeMonthChange" : null,
                              |  "sixMonthChange" : null,
                              |  "oneYearChange" : null,
                              |  "threeYearChange" : null,
                              |  "fiveYearChange" : null,
                              |  "tenYearChange" : null,
                              |  "maxChange" : 4.82
                              |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).findPerformanceSummary(AAPL, false)
      }

      "return 404 on not found" in {
        val svc = mocks
        when(svc.findPerformanceSummary(any[Ticker], anyBoolean)).thenRaiseError(AppError.PricePerformanceSummaryNotFound(AAPL))

        val res = for
          controller <- PriceController.make(svc)
          req = Request[IO](uri = uri"/price/performance-summary/AAPL", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find price performance summary for AAPL"}"""))
        verify(svc).findPerformanceSummary(AAPL, false)
      }

      "return 404 on not found even when fetchLatest is true" in {
        val svc = mocks
        when(svc.findPerformanceSummary(any[Ticker], anyBoolean)).thenRaiseError(AppError.PricePerformanceSummaryNotFound(AAPL))

        val res = for
          controller <- PriceController.make(svc)
          req = Request[IO](uri = uri"/price/performance-summary/AAPL?fetchLatest=true", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find price performance summary for AAPL"}"""))
        verify(svc).findPerformanceSummary(AAPL, true)
      }
    }
  }

  def mocks: PriceService[IO] = mock[PriceService[IO]]
}

