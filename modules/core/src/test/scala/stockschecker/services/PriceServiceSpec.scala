package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PricePerformanceSummary, Ticker}
import stockschecker.repositories.PricePerformanceSummaryRepository
import stockschecker.fixtures.*

class PriceServiceSpec extends IOWordSpec {

  "A PriceService" when {
    "fetchLatestPerformanceSummary" should {
      "fetch price candles and save performance summary" in {
        val (repo, client) = mocks
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(any[PricePerformanceSummary])).thenReturnUnit

        val res = for
          svc <- PriceService.make(repo, client)
          _   <- svc.fetchLatestPerformanceSummary(AAPL)
        yield ()

        res.asserting { _ =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(any[PricePerformanceSummary])
          succeed
        }
      }

      "handle errors when fetching price candles fails" in {
        val (repo, client) = mocks
        val error = new RuntimeException("API error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, client)
          _   <- svc.fetchLatestPerformanceSummary(AAPL)
        yield ()

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verifyNoInteractions(repo)
          result mustBe Left(error)
        }
      }

      "handle errors when saving to repository fails" in {
        val (repo, client) = mocks
        val error = new RuntimeException("Database error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(any[PricePerformanceSummary])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, client)
          _   <- svc.fetchLatestPerformanceSummary(AAPL)
        yield ()

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(any[PricePerformanceSummary])
          result mustBe Left(error)
        }
      }
    }
  }

  def mocks: (PricePerformanceSummaryRepository[IO], MarketDataClient[IO]) =
    (mock[PricePerformanceSummaryRepository[IO]], mock[MarketDataClient[IO]])
}

