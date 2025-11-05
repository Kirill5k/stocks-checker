package stockschecker.clients.alphavantage

import cats.effect.IO
import kirill5k.common.sttp.test.Sttp4WordSpec
import stockschecker.common.config.AlphaVantageClientConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.client4.testing.ResponseStub

import java.time.LocalDate

class AlphaVantageClientSpec extends Sttp4WordSpec {
  "An AlphaVantageClient" when {

    val config = AlphaVantageClientConfig("http://alphavantage.co", "api-key")

    "getMonthlyPriceCandles" should {
      "return list of price candles on success" in {
        val expectedParams = Map("function" -> "TIME_SERIES_MONTHLY", "symbol" -> "AAPL", "apikey" -> "api-key")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/query") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("alpha-vantage/monthly-data-success.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- AlphaVantageClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("AAPL"))
        yield candles

        result.asserting { candles =>
          candles.size must be > 0
          candles.head mustBe PriceCandle(
            date = LocalDate.parse("2025-10-27"),
            open = BigDecimal("255.0400"),
            high = BigDecimal("269.1200"),
            low = BigDecimal("244.0000"),
            close = BigDecimal("268.8100"),
            volume = 848467207L
          )
          candles.toList(1) mustBe PriceCandle(
            date = LocalDate.parse("2025-09-30"),
            open = BigDecimal("229.2500"),
            high = BigDecimal("257.6000"),
            low = BigDecimal("225.9500"),
            close = BigDecimal("254.6300"),
            volume = 1265170319L
          )
        }
      }

      "return error when API rate limit is exceeded" in {
        val expectedParams = Map("function" -> "TIME_SERIES_MONTHLY", "symbol" -> "AAPL", "apikey" -> "api-key")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/query") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("alpha-vantage/monthly-data-error.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- AlphaVantageClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("AAPL"))
        yield candles

        result.attempt.asserting(_ mustBe Left(AppError.Http(429, "All Alpha Vantage API keys exhausted due to rate limiting: api-key")))
      }

      "retry on 429 errors when one of the keys is exhausted" in {
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/query") && r.hasParams(Map("apikey" -> "key1")) =>
              ResponseStub.adjust(readJson("alpha-vantage/monthly-data-error.json"))
            case r if r.isGet && r.hasPath("/query") && r.hasParams(Map("apikey" -> "key2")) =>
              ResponseStub.adjust(readJson("alpha-vantage/monthly-data-success.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client <- AlphaVantageClient.make[IO](config.copy(apiKey = "key1,key2"), testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("AAPL"))
        yield candles

        result.asserting(_.size mustBe 311)
      }

      "return error when no time series data is returned" in {
        val expectedParams = Map("function" -> "TIME_SERIES_MONTHLY", "symbol" -> "INVALID", "apikey" -> "api-key")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/query") && r.hasParams(expectedParams) =>
              ResponseStub.adjust("""{}""")
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- AlphaVantageClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("INVALID"))
        yield candles

        result.attempt.asserting(_ mustBe Left(AppError.Http(500, "No time series data returned for ticker INVALID")))
      }

      "return error when empty candle data is returned" in {
        val expectedParams = Map("function" -> "TIME_SERIES_MONTHLY", "symbol" -> "EMPTY", "apikey" -> "api-key")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/query") && r.hasParams(expectedParams) =>
              ResponseStub.adjust("""{"Monthly Time Series": {}}""")
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- AlphaVantageClient.make[IO](config, testingBackend)
          candles <- client.getMonthlyPriceCandles(Ticker("EMPTY"))
        yield candles

        result.attempt.asserting(_ mustBe Left(AppError.Http(500, "No time series data returned for ticker EMPTY")))
      }
    }
  }
}
