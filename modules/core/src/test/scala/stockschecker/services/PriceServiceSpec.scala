package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PricePerformanceSummary, Ticker}
import stockschecker.repositories.PricePerformanceSummaryRepository
import stockschecker.fixtures.*

class PriceServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A PriceService" when {
    "fetchLatestPerformanceSummaries" should {
      "fetch price candles, save performance summary and dispatch Action.RecordPricePerformanceUpdate" in {
        val (repo, client, dispatcher) = mocks
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(anyList[PricePerformanceSummary])).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit

        val res = for
          svc <- PriceService.make(repo, client, dispatcher)
          _   <- svc.fetchLatestPerformanceSummaries(NonEmptyList.of(AAPL))
        yield ()

        res.asserting { _ =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(anyList[PricePerformanceSummary])
          verify(dispatcher).dispatch(Action.RecordPricePerformanceUpdate(List(AAPL)))
          succeed
        }
      }

      "ignore errors when fetching price candles fails" in {
        val (repo, client, dispatcher) = mocks
        val error = new RuntimeException("API error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, client, dispatcher)
          _   <- svc.fetchLatestPerformanceSummaries(NonEmptyList.of(AAPL))
        yield ()

        res.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verifyNoInteractions(repo)
          result mustBe ()
        }
      }

      "handle errors when saving to repository fails" in {
        val (repo, client, dispatcher) = mocks
        val error = new RuntimeException("Database error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(anyList[PricePerformanceSummary])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, client, dispatcher)
          _   <- svc.fetchLatestPerformanceSummaries(NonEmptyList.of(AAPL))
        yield ()

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(anyList[PricePerformanceSummary])
          result mustBe Left(error)
        }
      }
    }

    "findPerformanceSummary" should {
      "return performance summary from repository when found and fetch is false" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnIO(Some(AAPLPricePerformanceSummary))

        val res = for
          svc    <- PriceService.make(repo, client, dispatcher)
          result <- svc.findPerformanceSummary(AAPL, fetch = false)
        yield result

        res.asserting { result =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client, dispatcher)
          result mustBe AAPLPricePerformanceSummary
        }
      }

      "fetch from market data client when fetch is true" in {
        val (repo, client, dispatcher) = mocks
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(anyList[PricePerformanceSummary])).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc    <- PriceService.make(repo, client, dispatcher)
          result <- svc.findPerformanceSummary(AAPL, fetch = true)
        yield result

        res.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(anyList[PricePerformanceSummary])
          result.ticker.mustBe(AAPL)
        }
      }

      "raise error when not found in repository and fetch is false" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnIO(None)

        val res = for
          svc    <- PriceService.make(repo, client, dispatcher)
          result <- svc.findPerformanceSummary(AAPL, fetch = false)
        yield result

        res.attempt.asserting { result =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client)
          result.isLeft mustBe true
        }
      }

      "handle errors when market data client fails with fetch true" in {
        val (repo, client, dispatcher) = mocks
        val error = new RuntimeException("API error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenRaiseError(error)

        val res = for
          svc    <- PriceService.make(repo, client, dispatcher)
          result <- svc.findPerformanceSummary(AAPL, fetch = true)
        yield result

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          result mustBe Left(error)
        }
      }
    }
  }

  def mocks: (PricePerformanceSummaryRepository[IO], MarketDataClient[IO], ActionDispatcher[IO]) =
    (mock[PricePerformanceSummaryRepository[IO]], mock[MarketDataClient[IO]], mock[ActionDispatcher[IO]])
}

