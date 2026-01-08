package stockschecker.controllers

import cats.effect.IO
import cats.data.NonEmptyList
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{PriceAnalyticsFilter, Ticker, TimePeriod}
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
        when(svc.findPriceAnalytics(any[Ticker], anyBoolean)).thenReturnIO(AAPLPriceAnalytics)

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL?fetchLatest=true", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = s"""{
                              |  "ticker" : "AAPL",
                              |  "performanceSummary" : {
                              |    "latestPrice" : 228.50,
                              |    "latestPriceDate" : "2025-10-01",
                              |    "oneMonthChange" : 2.47,
                              |    "threeMonthChange" : null,
                              |    "sixMonthChange" : null,
                              |    "oneYearChange" : null,
                              |    "threeYearChange" : null,
                              |    "fiveYearChange" : null,
                              |    "tenYearChange" : null,
                              |    "maxChange" : 4.82
                              |  },
                              |  "metrics" : {
                              |    "cagr3Year" : null,
                              |    "cagr5Year" : null,
                              |    "volatility" : null,
                              |    "maxDrawdown" : 0,
                              |    "consistencyScore" : 0.9992,
                              |    "positiveYears" : 0,
                              |    "totalYears" : 0
                              |  },
                              |  "scores" : {
                              |    "overallScore" : 32.49,
                              |    "cagrScore" : 0,
                              |    "volatilityScore" : 0,
                              |    "drawdownScore" : 100,
                              |    "consistencyScore" : 49.96
                              |  }
                              |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).findPriceAnalytics(AAPL, true)
      }

      "return 200 and price performance summary when fetchLatest is not provided" in {
        val svc = mocks
        when(svc.findPriceAnalytics(any[Ticker], anyBoolean)).thenReturnIO(AAPLPriceAnalytics)

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = s"""{
                              |  "ticker" : "AAPL",
                              |  "performanceSummary" : {
                              |    "latestPrice" : 228.50,
                              |    "latestPriceDate" : "2025-10-01",
                              |    "oneMonthChange" : 2.47,
                              |    "threeMonthChange" : null,
                              |    "sixMonthChange" : null,
                              |    "oneYearChange" : null,
                              |    "threeYearChange" : null,
                              |    "fiveYearChange" : null,
                              |    "tenYearChange" : null,
                              |    "maxChange" : 4.82
                              |  },
                              |  "metrics" : {
                              |    "cagr3Year" : null,
                              |    "cagr5Year" : null,
                              |    "volatility" : null,
                              |    "maxDrawdown" : 0,
                              |    "consistencyScore" : 0.9992,
                              |    "positiveYears" : 0,
                              |    "totalYears" : 0
                              |  },
                              |  "scores" : {
                              |    "overallScore" : 32.49,
                              |    "cagrScore" : 0,
                              |    "volatilityScore" : 0,
                              |    "drawdownScore" : 100,
                              |    "consistencyScore" : 49.96
                              |  }
                              |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).findPriceAnalytics(AAPL, false)
      }

      "return 404 on not found" in {
        val svc = mocks
        when(svc.findPriceAnalytics(any[Ticker], anyBoolean)).thenRaiseError(AppError.PriceAnalyticsNotFound(AAPL))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find price analytics for AAPL"}"""))
        verify(svc).findPriceAnalytics(AAPL, false)
      }

      "return 404 on not found even when fetchLatest is true" in {
        val svc = mocks
        when(svc.findPriceAnalytics(any[Ticker], anyBoolean)).thenRaiseError(AppError.PriceAnalyticsNotFound(AAPL))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries/AAPL?fetchLatest=true", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find price analytics for AAPL"}"""))
        verify(svc).findPriceAnalytics(AAPL, true)
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
        when(svc.getAllPriceAnalytics(any[Option[Int]])).thenReturnIO(List.empty)

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some("[]"))
        verify(svc).getAllPriceAnalytics(None)
      }

      "return 200 with multiple summaries when no filters are provided" in {
        val svc = mocks
        val msftAnalytics = AAPLPriceAnalytics.copy(ticker = MSFT, performanceSummary = AAPLPricePerformanceSummary.copy(latestPrice = BigDecimal("400.00")))
        when(svc.getAllPriceAnalytics(any[Option[Int]])).thenReturnIO(List(AAPLPriceAnalytics, msftAnalytics))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
                             |  "ticker" : "AAPL",
                             |  "performanceSummary" : {
                             |    "latestPrice" : 228.50,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |},{
                             |  "ticker" : "MSFT",
                             |  "performanceSummary" : {
                             |    "latestPrice" : 400.00,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAllPriceAnalytics(None)
      }

      "return 200 with filtered summaries when minLatestPrice filter is provided" in {
        val svc = mocks
        when(svc.findPriceAnalyticsBy(any[PriceAnalyticsFilter], any[Option[Int]])).thenReturnIO(List(AAPLPriceAnalytics))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?minLatestPrice=200", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
                             |  "ticker" : "AAPL",
                             |  "performanceSummary" : {
                             |    "latestPrice" : 228.50,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PriceAnalyticsFilter.Composite(NonEmptyList.one(PriceAnalyticsFilter.PriceAbove(BigDecimal(200))))
        verify(svc).findPriceAnalyticsBy(expectedFilter, None)
      }

      "return 200 with filtered summaries when maxLatestPrice filter is provided" in {
        val svc = mocks
        when(svc.findPriceAnalyticsBy(any[PriceAnalyticsFilter], any[Option[Int]])).thenReturnIO(List(AAPLPriceAnalytics))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?maxLatestPrice=300", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
                             |  "ticker" : "AAPL",
                             |  "performanceSummary" : {
                             |    "latestPrice" : 228.50,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PriceAnalyticsFilter.Composite(NonEmptyList.one(PriceAnalyticsFilter.PriceBelow(BigDecimal(300))))
        verify(svc).findPriceAnalyticsBy(expectedFilter, None)
      }

      "return 200 with filtered summaries when minOneYearChange filter is provided" in {
        val svc = mocks
        when(svc.findPriceAnalyticsBy(any[PriceAnalyticsFilter], any[Option[Int]])).thenReturnIO(List(AAPLPriceAnalytics))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?minOneYearChange=10.5", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
                             |  "ticker" : "AAPL",
                             |  "performanceSummary" : {
                             |    "latestPrice" : 228.50,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PriceAnalyticsFilter.Composite(NonEmptyList.one(PriceAnalyticsFilter.PerformanceAbove(TimePeriod.OneYear, BigDecimal(10.5))))
        verify(svc).findPriceAnalyticsBy(expectedFilter, None)
      }

      "return 200 with filtered summaries when multiple filters are provided" in {
        val svc = mocks
        when(svc.findPriceAnalyticsBy(any[PriceAnalyticsFilter], any[Option[Int]])).thenReturnIO(List(AAPLPriceAnalytics))

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
                             |  "performanceSummary" : {
                             |    "latestPrice" : 228.50,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PriceAnalyticsFilter.Composite(
          NonEmptyList.of(
            PriceAnalyticsFilter.PriceAbove(BigDecimal(200)),
            PriceAnalyticsFilter.PriceBelow(BigDecimal(300)),
            PriceAnalyticsFilter.PerformanceAbove(TimePeriod.OneMonth, BigDecimal(1.0)),
            PriceAnalyticsFilter.PerformanceBelow(TimePeriod.OneYear, BigDecimal(50.0))
          )
        )
        verify(svc).findPriceAnalyticsBy(expectedFilter, None)
      }

      "return 200 with limited results when limit parameter is provided without filters" in {
        val svc = mocks
        when(svc.getAllPriceAnalytics(any[Option[Int]])).thenReturnIO(List(AAPLPriceAnalytics))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?limit=10", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
                             |  "ticker" : "AAPL",
                             |  "performanceSummary" : {
                             |    "latestPrice" : 228.50,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAllPriceAnalytics(Some(10))
      }

      "return 200 with limited results when limit parameter is provided with filters" in {
        val svc = mocks
        when(svc.findPriceAnalyticsBy(any[PriceAnalyticsFilter], any[Option[Int]])).thenReturnIO(List(AAPLPriceAnalytics))

        val res = for
          controller <- PriceController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/price/performance-summaries?minLatestPrice=200&limit=5", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody = """[{
                             |  "ticker" : "AAPL",
                             |  "performanceSummary" : {
                             |    "latestPrice" : 228.50,
                             |    "latestPriceDate" : "2025-10-01",
                             |    "oneMonthChange" : 2.47,
                             |    "threeMonthChange" : null,
                             |    "sixMonthChange" : null,
                             |    "oneYearChange" : null,
                             |    "threeYearChange" : null,
                             |    "fiveYearChange" : null,
                             |    "tenYearChange" : null,
                             |    "maxChange" : 4.82
                             |  },
                             |  "metrics" : {
                             |    "cagr3Year" : null,
                             |    "cagr5Year" : null,
                             |    "volatility" : null,
                             |    "maxDrawdown" : 0,
                             |    "consistencyScore" : 0.9992,
                             |    "positiveYears" : 0,
                             |    "totalYears" : 0
                             |  },
                             |  "scores" : {
                             |    "overallScore" : 32.49,
                             |    "cagrScore" : 0,
                             |    "volatilityScore" : 0,
                             |    "drawdownScore" : 100,
                             |    "consistencyScore" : 49.96
                             |  }
                             |}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        val expectedFilter = PriceAnalyticsFilter.Composite(NonEmptyList.one(PriceAnalyticsFilter.PriceAbove(BigDecimal(200))))
        verify(svc).findPriceAnalyticsBy(expectedFilter, Some(5))
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