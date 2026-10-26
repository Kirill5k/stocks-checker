package stockschecker.clients.twelvedata

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import kirill5k.common.sttp.test.Sttp4WordSpec
import stockschecker.common.config.TwelveDataConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.client4.testing.ResponseStub
import sttp.model.{Header, StatusCode}

import java.time.LocalDate
import scala.concurrent.duration.*

class TwelveDataClientSpec extends Sttp4WordSpec {
  private def dailyResponse(
      values: List[(LocalDate, BigDecimal)],
      symbol: String = "AAPL",
      interval: String = "1day",
      currency: String = "USD",
      metadata: Boolean = true
  ): String = TwelveDataClient
    .TimeSeriesResponse(
      Option.when(metadata)(TwelveDataClient.Meta(symbol, interval, currency, "America/New_York", "NASDAQ", "XNAS", "Common Stock")),
      Some(values.map { case (date, close) => TwelveDataClient.CandleData(date, close, close, close, close, BigDecimal(1000)) }),
      "ok",
      None,
      None
    )
    .asJson
    .noSpaces

  "A TwelveDataClient" when {

    val config = TwelveDataConfig("http://twelvedata.co", "api-key")

    "getDailyPriceHistory" should {
      val from    = LocalDate.parse("2025-01-02")
      val through = LocalDate.parse("2025-01-06")
      val candles = List(through -> BigDecimal(102), from -> BigDecimal(100))

      "request the explicit adjustment and preserve provider currency and request provenance" in
        List("all" -> "split-and-dividend", "none" -> "none", "splits" -> "splits")
          .traverse { case (requested, stored) =>
            val expectedParams = Map(
              "symbol"     -> "AAPL",
              "interval"   -> "1day",
              "apikey"     -> "api-key",
              "outputsize" -> "5000",
              "start_date" -> from.toString,
              "end_date"   -> through.toString,
              "adjust"     -> requested
            )
            val backend = fs2BackendStub.whenRequestMatchesPartial {
              case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
                ResponseStub.adjust(dailyResponse(candles, currency = "EUR"))
              case _ => throw new RuntimeException("Unexpected daily history request")
            }
            for {
              client <- TwelveDataClient.make[IO](config, backend)
              result <- client.getDailyPriceHistory(Ticker("AAPL"), from, through, requested)
            } yield (stored, result)
          }
          .asserting { results =>
            results.foreach { case (adjustment, result) =>
              result.ticker mustBe Ticker("AAPL")
              result.currency mustBe "EUR"
              result.provider mustBe "TwelveData"
              result.adjustment mustBe adjustment
              result.requestedFrom mustBe from
              result.requestedThrough mustBe through
              result.candles.map(_.date) mustBe List(from, through)
              result.candles.map(_.close) mustBe List(BigDecimal(100), BigDecimal(102))
            }
            succeed
          }

      "reject incorrect identity, interval, or missing currency metadata" in
        List(
          "wrong symbol"     -> dailyResponse(candles, symbol = "MSFT"),
          "wrong interval"   -> dailyResponse(candles, interval = "1month"),
          "missing metadata" -> dailyResponse(candles, metadata = false)
        ).traverse { case (label, json) =>
          val backend = fs2BackendStub.whenRequestMatchesPartial { case _ => ResponseStub.adjust(json) }
          TwelveDataClient
            .make[IO](config, backend)
            .flatMap(_.getDailyPriceHistory(Ticker("AAPL"), from, through, "all"))
            .attempt
            .map(label -> _)
        }.asserting { results =>
          results.foreach { case (label, result) =>
            withClue(label)(result.left.toOption.get mustBe a[AppError.FailedValidation])
          }
          succeed
        }

      "reject duplicate, nonpositive, out-of-window, gapped, and possibly truncated histories" in {
        val start   = LocalDate.parse("2000-01-01")
        val end     = LocalDate.parse("2030-01-01")
        val invalid = List(
          "duplicate"      -> List(start -> BigDecimal(1), start -> BigDecimal(2)),
          "zero"           -> List(start -> BigDecimal(0)),
          "negative"       -> List(start -> BigDecimal(-1)),
          "outside window" -> List(start.minusDays(1) -> BigDecimal(1)),
          "gap"            -> List(start -> BigDecimal(1), start.plusDays(8) -> BigDecimal(2)),
          "truncated"      -> (0 until 5000).toList.map(i => start.plusDays(i.toLong) -> BigDecimal(1))
        )
        invalid
          .traverse { case (label, values) =>
            val backend = fs2BackendStub.whenRequestMatchesPartial { case _ => ResponseStub.adjust(dailyResponse(values)) }
            TwelveDataClient
              .make[IO](config, backend)
              .flatMap(_.getDailyPriceHistory(Ticker("AAPL"), start, end, "all"))
              .attempt
              .map(label -> _)
          }
          .asserting { results =>
            results.foreach { case (label, result) =>
              withClue(label)(result.left.toOption.get mustBe a[AppError.FailedValidation])
            }
            succeed
          }
      }

      "return a persisted-work deferral for HTTP 429 with Retry-After without retrying internally" in {
        val backend = fs2BackendStub.whenAnyRequest.thenRespondCyclic(
          ResponseStub.adjust("rate limited", StatusCode.TooManyRequests).copy(headers = Seq(Header("Retry-After", "120"))),
          ResponseStub.adjust(dailyResponse(candles))
        )
        (for {
          client <- TwelveDataClient.make[IO](config, backend)
          before <- IO.realTimeInstant
          first  <- client.getDailyPriceHistory(Ticker("AAPL"), from, through, "all").attempt.timeout(1.second)
          after  <- IO.realTimeInstant
          second <- client.getDailyPriceHistory(Ticker("AAPL"), from, through, "all")
        } yield (before, after, first, second)).asserting { case (before, after, first, second) =>
          val error = first.left.toOption.get.asInstanceOf[AppError.ProviderDeferred]
          error.provider mustBe "TwelveData"
          error.retryAt.isBefore(before.plusSeconds(120)) mustBe false
          error.retryAt.isAfter(after.plusSeconds(120)) mustBe false
          second.candles.size mustBe 2
        }
      }

      "defer HTTP-200 error bodies with code 429 without retrying internally" in {
        val backend = fs2BackendStub.whenAnyRequest.thenRespondCyclic(
          ResponseStub.adjust("""{"code":429,"status":"error","message":"rate limited"}"""),
          ResponseStub.adjust(dailyResponse(candles))
        )
        (for {
          client <- TwelveDataClient.make[IO](config, backend)
          first  <- client.getDailyPriceHistory(Ticker("AAPL"), from, through, "all").attempt.timeout(1.second)
          second <- client.getDailyPriceHistory(Ticker("AAPL"), from, through, "all")
        } yield (first, second)).asserting { case (first, second) =>
          first.left.toOption.get mustBe a[AppError.ProviderDeferred]
          second.candles.size mustBe 2
        }
      }

      "reject invalid date ranges and adjustment modes before issuing requests" in {
        val backend = fs2BackendStub.whenRequestMatchesPartial { case _ =>
          throw new RuntimeException("An invalid request reached the provider")
        }
        (for {
          client     <- TwelveDataClient.make[IO](config, backend)
          range      <- client.getDailyPriceHistory(Ticker("AAPL"), through, from, "all").attempt
          adjustment <- client.getDailyPriceHistory(Ticker("AAPL"), from, through, "invalid").attempt
        } yield List(range, adjustment)).asserting { results =>
          results.foreach(_.left.toOption.get mustBe a[AppError.FailedValidation])
          succeed
        }
      }
    }

    "getMonthlyPriceCandles" should {
      "return list of price candles on success" in {
        val expectedParams = Map("symbol" -> "AAPL", "interval" -> "1month", "apikey" -> "api-key", "outputsize" -> "150")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("twelve-data/monthly-data-success.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- TwelveDataClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("AAPL"))
        yield candles

        result.asserting { candles =>
          candles.size must be > 0
          candles.head mustBe PriceCandle(
            date = LocalDate.parse("2025-11-01"),
            open = BigDecimal("270.42001"),
            high = BigDecimal("271.70001"),
            low = BigDecimal("266.25"),
            close = BigDecimal("270.14001"),
            volume = 139848687L
          )
          candles.toList(1) mustBe PriceCandle(
            date = LocalDate.parse("2025-10-01"),
            open = BigDecimal("255.039993"),
            high = BigDecimal("277.32001"),
            low = BigDecimal("244"),
            close = BigDecimal("270.37000"),
            volume = 1097142200L
          )
        }
      }

      "return error when status is not ok" in {
        val expectedParams = Map("symbol" -> "INVALID", "interval" -> "1month", "apikey" -> "api-key", "outputsize" -> "150")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
              ResponseStub.adjust("""{"status": "error"}""")
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- TwelveDataClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("INVALID"))
        yield candles

        result.assertThrows(
          AppError.HttpClient("TwelveData", 500, "API error for getting time series data for INVALID: Unknown error from TwelveData API")
        )
      }

      "return error when no values are returned" in {
        val expectedParams = Map("symbol" -> "INVALID", "interval" -> "1month", "apikey" -> "api-key", "outputsize" -> "150")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
              ResponseStub.adjust("""{"status": "ok", "values": []}""")
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- TwelveDataClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("INVALID"))
        yield candles

        result.attempt.asserting(_ mustBe Left(AppError.HttpClient("TwelveData", 500, "No values returned for ticker INVALID")))
      }

      "return error when API returns status 200 with error in response body" in {
        val expectedParams = Map("symbol" -> "INVALID", "interval" -> "1month", "apikey" -> "api-key", "outputsize" -> "150")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(
                """{"code": 404, "message": "**symbol** or **figi** parameter is missing or invalid. Please provide a valid symbol according to API documentation: https://twelvedata.com/docs#reference-data", "status": "error"}"""
              )
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- TwelveDataClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("INVALID"))
        yield candles

        result.assertThrows(
          AppError.HttpClient(
            "TwelveData",
            404,
            "API error for getting time series data for INVALID: **symbol** or **figi** parameter is missing or invalid. Please provide a valid symbol according to API documentation: https://twelvedata.com/docs#reference-data"
          )
        )
      }

      "return error with default code when API returns error status without code" in {
        val expectedParams = Map("symbol" -> "INVALID", "interval" -> "1month", "apikey" -> "api-key", "outputsize" -> "150")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(
                """{"message": "Some error occurred", "status": "error"}"""
              )
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- TwelveDataClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("INVALID"))
        yield candles

        result.assertThrows(
          AppError.HttpClient("TwelveData", 500, "API error for getting time series data for INVALID: Some error occurred")
        )
      }

      "return error with default message when API returns error status without message" in {
        val expectedParams = Map("symbol" -> "INVALID", "interval" -> "1month", "apikey" -> "api-key", "outputsize" -> "150")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/time_series") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(
                """{"code": 400, "status": "error"}"""
              )
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- TwelveDataClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("INVALID"))
        yield candles

        result.assertThrows(
          AppError.HttpClient("TwelveData", 400, "API error for getting time series data for INVALID: Unknown error from TwelveData API")
        )
      }
    }
  }
}
