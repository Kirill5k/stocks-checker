package stockschecker.controllers

import cats.effect.IO
import cats.data.NonEmptyList
import io.circe.Json
import io.circe.parser.parse
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{PriceAnalyticsFilter, TimePeriod}
import stockschecker.domain.errors.AppError
import stockschecker.services.PriceService
import stockschecker.fixtures.*

class PriceControllerSpec extends HttpRoutesWordSpec {
  private val apiConfig    = ApiConfig("test-api-key")
  private val apiKey       = Header.Raw(CIString("X-API-Key"), apiConfig.key)
  private val responseBody = """{
    "ticker":"AAPL",
    "performanceSummary":{"latestPrice":228.50,"latestPriceDate":"2025-10-01","oneMonthChange":2.47,
      "threeMonthChange":null,"sixMonthChange":null,"oneYearChange":null,"threeYearChange":null,
      "fiveYearChange":null,"tenYearChange":null,"maxChange":4.82},
    "basis":{"purpose":"historical-performance","returnAdjustment":"split","drawdownFrequency":"monthly",
      "riskWindowYears":5,"priceObservationId":null,"quotedPriceObservationId":null},
    "metrics":{"cagr3Year":null,"cagr5Year":null,"volatility":null,"maxDrawdown":0,"consistencyScore":0.9992,"positiveYears":0,"totalYears":0},
    "scores":{"overallScore":32.49,"cagrScore":0,"volatilityScore":0,"drawdownScore":100,"consistencyScore":49.96}
  }"""

  "Price endpoints" should {
    List(false, true).foreach { fetch =>
      s"return analytics with fetchLatest=$fetch" in {
        val service = mock[PriceService[IO]]
        when(service.findPriceAnalytics(AAPL, fetch)).thenReturnIO(AAPLPriceAnalytics)
        val response = for
          controller <- PriceController.make(service, apiConfig)
          uri = if fetch then uri"/price/performance-summaries/AAPL?fetchLatest=true" else uri"/price/performance-summaries/AAPL"
          result <- controller.routes.orNotFound.run(Request[IO](uri = uri).withHeaders(apiKey))
        yield result
        response mustHaveStatus (Status.Ok, Some(responseBody))
        verify(service).findPriceAnalytics(AAPL, fetch)
      }

      s"return 404 for missing analytics with fetchLatest=$fetch" in {
        val service = mock[PriceService[IO]]
        when(service.findPriceAnalytics(AAPL, fetch)).thenRaiseError(AppError.PriceAnalyticsNotFound(AAPL))
        val response = for
          controller <- PriceController.make(service, apiConfig)
          result     <- controller.routes.orNotFound.run(
            Request[IO](uri = uri"/price/performance-summaries/AAPL".withQueryParam("fetchLatest", fetch)).withHeaders(apiKey)
          )
        yield result
        response mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find price analytics for AAPL"}"""))
      }
    }

    "return stored analytics with null basis without requesting a refresh" in {
      val service = mock[PriceService[IO]]
      when(service.findPriceAnalytics(AAPL, false)).thenReturnIO(AAPLPriceAnalytics.copy(basis = None))
      val result = for
        controller <- PriceController.make(service, apiConfig)
        response   <- controller.routes.orNotFound.run(Request[IO](uri = uri"/price/performance-summaries/AAPL").withHeaders(apiKey))
        body       <- response.as[String]
        json       <- IO.fromEither(parse(body))
      yield
        response.status mustBe Status.Ok
        json.hcursor.downField("basis").focus mustBe Some(Json.Null)
        json.hcursor.downField("performanceSummary").get[BigDecimal]("latestPrice") mustBe Right(AAPLPricePerformanceSummary.latestPrice)
        verify(service).findPriceAnalytics(AAPL, false)
        succeed
      result.unsafeToFuture()(using cats.effect.unsafe.IORuntime.global)
    }

    val priceFilters = List(
      "minLatestPrice" -> PriceAnalyticsFilter.PriceAbove(BigDecimal("10.5")),
      "maxLatestPrice" -> PriceAnalyticsFilter.PriceBelow(BigDecimal("10.5"))
    )
    val performanceFilters = List(
      "OneMonth"   -> TimePeriod.OneMonth,
      "ThreeMonth" -> TimePeriod.ThreeMonth,
      "SixMonth"   -> TimePeriod.SixMonth,
      "OneYear"    -> TimePeriod.OneYear,
      "ThreeYear"  -> TimePeriod.ThreeYear,
      "FiveYear"   -> TimePeriod.FiveYear,
      "TenYear"    -> TimePeriod.TenYear,
      "Max"        -> TimePeriod.Max
    ).flatMap { case (name, period) =>
      List(
        s"min${name}Change" -> PriceAnalyticsFilter.PerformanceAbove(period, BigDecimal("10.5")),
        s"max${name}Change" -> PriceAnalyticsFilter.PerformanceBelow(period, BigDecimal("10.5"))
      )
    }
    val cases = (priceFilters ++ performanceFilters).map { case (name, filter) =>
      (s"$name=10.5", Some(PriceAnalyticsFilter.Composite(NonEmptyList.one(filter))), None)
    } ++ List(
      ("", None, None),
      ("limit=10", None, Some(10)),
      ("minLatestPrice=10.5&limit=5", Some(PriceAnalyticsFilter.Composite(NonEmptyList.one(priceFilters.head._2))), Some(5)),
      (
        "minLatestPrice=200&maxLatestPrice=300&minOneMonthChange=1&maxOneYearChange=50",
        Some(
          PriceAnalyticsFilter.Composite(
            NonEmptyList.of(
              PriceAnalyticsFilter.PriceAbove(200),
              PriceAnalyticsFilter.PriceBelow(300),
              PriceAnalyticsFilter.PerformanceAbove(TimePeriod.OneMonth, 1),
              PriceAnalyticsFilter.PerformanceBelow(TimePeriod.OneYear, 50)
            )
          )
        ),
        None
      )
    )
    cases.foreach { case (query, filter, limit) =>
      s"decode filters and limits for '$query'" in {
        val service = mock[PriceService[IO]]
        when(service.findPriceAnalyticsBy(filter, limit)).thenReturnIO(List(AAPLPriceAnalytics))
        val response = for
          controller <- PriceController.make(service, apiConfig)
          result     <- controller.routes.orNotFound.run(
            Request[IO](uri = Uri.unsafeFromString(s"/price/performance-summaries?$query")).withHeaders(apiKey)
          )
        yield result
        response mustHaveStatus (Status.Ok, Some(s"[$responseBody]"))
        verify(service).findPriceAnalyticsBy(filter, limit)
      }
    }

    "return an empty collection when no analytics match" in {
      val service = mock[PriceService[IO]]
      when(service.findPriceAnalyticsBy(None, None)).thenReturnIO(Nil)
      val response = for
        controller <- PriceController.make(service, apiConfig)
        result     <- controller.routes.orNotFound.run(Request[IO](uri = uri"/price/performance-summaries").withHeaders(apiKey))
      yield result
      response mustHaveStatus (Status.Ok, Some("[]"))
    }

    List(0, 101).foreach { limit =>
      s"reject limit=$limit before reading analytics" in {
        val service  = mock[PriceService[IO]]
        val response = for
          controller <- PriceController.make(service, apiConfig)
          result     <- controller.routes.orNotFound.run(
            Request[IO](uri = uri"/price/performance-summaries".withQueryParam("limit", limit)).withHeaders(apiKey)
          )
        yield result
        response
          .map { result => result.status mustBe Status.UnprocessableContent; verifyNoInteractions(service); succeed }
          .unsafeToFuture()(using cats.effect.unsafe.IORuntime.global)
      }
    }
  }
}
