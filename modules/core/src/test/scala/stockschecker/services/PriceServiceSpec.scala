package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PriceAnalytics, Ticker}
import stockschecker.repositories.{LatestPriceRepository, PriceAnalyticsRepository}
import stockschecker.fixtures.*

class PriceServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A PriceService" when {
    "fetchLatestPriceAnalytics" should {
      "fetch price candles, save price analytics and dispatch Action.RecordPriceAnalyticsUpdate" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(anyList[PriceAnalytics])).thenReturnUnit
        when(latestPriceRepo.save(anyList)).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit

        val res = for
          svc <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          _   <- svc.fetchLatestPriceAnalytics(NonEmptyList.of(AAPL))
        yield ()

        res.asserting { _ =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(anyList[PriceAnalytics])
          verify(latestPriceRepo).save(anyList)
          verify(dispatcher).dispatch(Action.RecordPriceAnalyticsUpdate(List(AAPL)))
          succeed
        }
      }

      "ignore errors when fetching price candles fails" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val error                                       = new RuntimeException("API error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          _   <- svc.fetchLatestPriceAnalytics(NonEmptyList.of(AAPL))
        yield ()

        res.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verifyNoInteractions(repo, latestPriceRepo)
          result mustBe ()
        }
      }

      "handle errors when saving to repository fails" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val error                                       = new RuntimeException("Database error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(anyList[PriceAnalytics])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          _   <- svc.fetchLatestPriceAnalytics(NonEmptyList.of(AAPL))
        yield ()

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(anyList[PriceAnalytics])
          result mustBe Left(error)
        }
      }
    }

    "findPriceAnalytics" should {
      "return price analytics from repository when found and fetch is false" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnIO(Some(AAPLPriceAnalytics))

        val res = for
          svc    <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          result <- svc.findPriceAnalytics(AAPL, fetch = false)
        yield result

        res.asserting { result =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client, dispatcher)
          result mustBe AAPLPriceAnalytics
        }
      }

      "fetch from market data client when fetch is true" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        when(client.getMonthlyPriceCandles(any[Ticker])).thenReturnIO(AAPLPriceCandles)
        when(repo.save(anyList[PriceAnalytics])).thenReturnUnit
        when(latestPriceRepo.save(anyList)).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc    <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          result <- svc.findPriceAnalytics(AAPL, fetch = true)
        yield result

        res.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verify(repo).save(anyList[PriceAnalytics])
          verify(latestPriceRepo).save(anyList)
          result.ticker.mustBe(AAPL)
        }
      }

      "raise error when not found in repository and fetch is false" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnIO(None)

        val res = for
          svc    <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          result <- svc.findPriceAnalytics(AAPL, fetch = false)
        yield result

        res.attempt.asserting { result =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client)
          result.isLeft mustBe true
        }
      }

      "handle errors when market data client fails with fetch true" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val error                                       = new RuntimeException("API error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenRaiseError(error)

        val res = for
          svc    <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          result <- svc.findPriceAnalytics(AAPL, fetch = true)
        yield result

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          result mustBe Left(error)
        }
      }
    }
  }

  def mocks: (PriceAnalyticsRepository[IO], LatestPriceRepository[IO], MarketDataClient[IO], ActionDispatcher[IO]) =
    (mock[PriceAnalyticsRepository[IO]], mock[LatestPriceRepository[IO]], mock[MarketDataClient[IO]], mock[ActionDispatcher[IO]])
}
