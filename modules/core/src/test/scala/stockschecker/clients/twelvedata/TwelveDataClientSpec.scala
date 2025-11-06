package stockschecker.clients.twelvedata

import cats.effect.IO
import kirill5k.common.sttp.test.Sttp4WordSpec
import stockschecker.common.config.TwelveDataConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.client4.testing.ResponseStub

import java.time.LocalDate

class TwelveDataClientSpec extends Sttp4WordSpec {
  "A TwelveDataClient" when {

    val config = TwelveDataConfig("http://twelvedata.co", "api-key")

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

        result.attempt.asserting(_ mustBe Left(AppError.Http(500, "TwelveData API returned status: error")))
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

        result.attempt.asserting(_ mustBe Left(AppError.Http(500, "No values returned for ticker INVALID")))
      }
    }
  }
}

