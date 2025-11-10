package stockschecker.controllers

import cats.effect.IO
import cats.data.NonEmptyList
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{PricePerformanceFilter, Ticker, TimePeriod}
import stockschecker.domain.errors.AppError
import stockschecker.services.PriceService
import stockschecker.fixtures.*

class PriceControllerSpec extends HttpRoutesWordSpec {

  val testApiKey   = "test-api-key-12345"
  val apiConfig    = ApiConfig(testApiKey)
  val apiKeyHeader = Header.Raw(CIString("X-API-Key"), testApiKey)

  "A PriceController" when {
    "GET /price/performance-summaries/:ticker" should {
      "return 200 and price performance summary on success" in {
        val svc = mocks
        when(svc.findPerformanceSummary(any[Ticker], anyBoolean)).thenReturnIO(AAPLPricePerformanceSummary)

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL?fetchLatest=true", method = Method.GET).withHeaders(apiKeyHeader)
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
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL", method = Method.GET).withHeaders(apiKeyHeader)
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
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find price performance summary for AAPL"}"""))
        verify(svc).findPerformanceSummary(AAPL, false)
      }

      "return 404 on not found even when fetchLatest is true" in {
        val svc = mocks
        when(svc.findPerformanceSummary(any[Ticker], anyBoolean)).thenRaiseError(AppError.PricePerformanceSummaryNotFound(AAPL))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL?fetchLatest=true", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find price performance summary for AAPL"}"""))
        verify(svc).findPerformanceSummary(AAPL, true)
      }

      "return 401 when API key is missing" in {
        val svc = mocks

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }

      "return 401 when API key is invalid" in {
        val svc = mocks

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          invalidApiKeyHeader = Header.Raw(CIString("X-API-Key"), "wrong-key")
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL", method = Method.GET).withHeaders(invalidApiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }
    }

    "GET /price/performance-summaries" should {
      "return 200 with empty list when no summaries exist" in {
        val svc = mocks
        when(svc.getAllPerformanceSummaries(any[Option[Int]])).thenReturnIO(List.empty)

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some("[]"))
        verify(svc).getAllPerformanceSummaries(None)
      }

      "return 200 with multiple summaries when no filters are provided" in {
        val svc = mocks
        val msftSummary = AAPLPricePerformanceSummary.copy(ticker = MSFT, latestPrice = BigDecimal("400.00"))
        when(svc.getAllPerformanceSummaries(any[Option[Int]])).thenReturnIO(List(AAPLPricePerformanceSummary, msftSummary))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
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
                             |},{
                             |  "ticker" : "MSFT",
                             |  "latestPrice" : 400.00,
                             |  "latestPriceDate" : "2025-10-01",
                             |  "oneMonthChange" : 2.47,
                             |  "threeMonthChange" : null,
                             |  "sixMonthChange" : null,
                             |  "oneYearChange" : null,
                             |  "threeYearChange" : null,
                             |  "fiveYearChange" : null,
                             |  "tenYearChange" : null,
                             |  "maxChange" : 4.82
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAllPerformanceSummaries(None)
      }

      "return 200 with filtered summaries when minLatestPrice filter is provided" in {
        val svc = mocks
        when(svc.findPerformanceSummariesBy(any[PricePerformanceFilter], any[Option[Int]])).thenReturnIO(List(AAPLPricePerformanceSummary))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?minLatestPrice=200", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
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
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PricePerformanceFilter.Composite(NonEmptyList.one(PricePerformanceFilter.PriceAbove(BigDecimal(200))))
        verify(svc).findPerformanceSummariesBy(expectedFilter, None)
      }

      "return 200 with filtered summaries when maxLatestPrice filter is provided" in {
        val svc = mocks
        when(svc.findPerformanceSummariesBy(any[PricePerformanceFilter], any[Option[Int]])).thenReturnIO(List(AAPLPricePerformanceSummary))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?maxLatestPrice=300", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
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
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PricePerformanceFilter.Composite(NonEmptyList.one(PricePerformanceFilter.PriceBelow(BigDecimal(300))))
        verify(svc).findPerformanceSummariesBy(expectedFilter, None)
      }

      "return 200 with filtered summaries when minOneYearChange filter is provided" in {
        val svc = mocks
        when(svc.findPerformanceSummariesBy(any[PricePerformanceFilter], any[Option[Int]])).thenReturnIO(List(AAPLPricePerformanceSummary))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?minOneYearChange=10.5", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
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
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PricePerformanceFilter.Composite(NonEmptyList.one(PricePerformanceFilter.PerformanceAbove(TimePeriod.OneYear, BigDecimal(10.5))))
        verify(svc).findPerformanceSummariesBy(expectedFilter, None)
      }

      "return 200 with filtered summaries when multiple filters are provided" in {
        val svc = mocks
        when(svc.findPerformanceSummariesBy(any[PricePerformanceFilter], any[Option[Int]])).thenReturnIO(List(AAPLPricePerformanceSummary))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](
            uri = uri"/price/performance-summaries?minLatestPrice=200&maxLatestPrice=300&minOneMonthChange=1.0&maxOneYearChange=50.0",
            method = Method.GET
          ).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
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
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PricePerformanceFilter.Composite(
          NonEmptyList.of(
            PricePerformanceFilter.PriceAbove(BigDecimal(200)),
            PricePerformanceFilter.PriceBelow(BigDecimal(300)),
            PricePerformanceFilter.PerformanceAbove(TimePeriod.OneMonth, BigDecimal(1.0)),
            PricePerformanceFilter.PerformanceBelow(TimePeriod.OneYear, BigDecimal(50.0))
          )
        )
        verify(svc).findPerformanceSummariesBy(expectedFilter, None)
      }

      "return 200 with limited results when limit parameter is provided without filters" in {
        val svc = mocks
        when(svc.getAllPerformanceSummaries(any[Option[Int]])).thenReturnIO(List(AAPLPricePerformanceSummary))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?limit=10", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
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
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAllPerformanceSummaries(Some(10))
      }

      "return 200 with limited results when limit parameter is provided with filters" in {
        val svc = mocks
        when(svc.findPerformanceSummariesBy(any[PricePerformanceFilter], any[Option[Int]])).thenReturnIO(List(AAPLPricePerformanceSummary))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?minLatestPrice=200&limit=5", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
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
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PricePerformanceFilter.Composite(NonEmptyList.one(PricePerformanceFilter.PriceAbove(BigDecimal(200))))
        verify(svc).findPerformanceSummariesBy(expectedFilter, Some(5))
      }

      "return 401 when API key is missing" in {
        val svc = mocks

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }

      "return 401 when API key is invalid" in {
        val svc = mocks

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          invalidApiKeyHeader = Header.Raw(CIString("X-API-Key"), "wrong-key")
          req = Request[IO](uri = uri"/price/performance-summaries", method = Method.GET).withHeaders(invalidApiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }
    }
  }

  def mocks = mock[PriceService[IO]]
}
