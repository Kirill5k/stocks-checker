package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{FinancialData, FinancialMetrics, FinancialSnapshot, InvestmentScorer, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{FinancialHistoryRepository, FinancialMetricsRepository}
import stockschecker.fixtures.*

class FinancialMetricsServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A FinancialMetricsService" when {
    "fetchLatest" should {
      "persist an immutable financial vintage and propagate a later provider deferral" in {
        val (repo, client, dispatcher) = mocks
        val history                    = mock[FinancialHistoryRepository[IO]]
        val snapshot                   =
          FinancialSnapshot("vintage", AAPL, "Finnhub", ts, Map.empty, Nil, Nil, "{}", "hash", InvestmentScorer.MappingVersion, Nil)
        val deferred = AppError.ProviderDeferred("Finnhub", ts.plusSeconds(60), "rate-limited")
        when(client.getFinancialData(AAPL)).thenReturnIO(Some(FinancialData(AAPLFinancialMetrics, snapshot)))
        when(client.getFinancialData(MSFT)).thenRaiseError(deferred)
        when(history.save(snapshot)).thenReturnUnit
        when(repo.save(anyList[FinancialMetrics])).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit
        val res = for
          svc <- FinancialMetricsService.make(repo, client, dispatcher, Some(history))
          _   <- svc.fetchLatest(NonEmptyList.of(AAPL, MSFT))
        yield ()
        res.attempt.asserting { result =>
          result mustBe Left(deferred)
          verify(history).save(snapshot)
          verify(repo).save(List(AAPLFinancialMetrics))
          verify(dispatcher).dispatch(Action.RecordFinancialMetricsUpdate(List(AAPL)))
          succeed
        }
      }

      "not publish latest financial metrics when immutable history cannot be saved" in {
        val (repo, client, dispatcher) = mocks
        val history                    = mock[FinancialHistoryRepository[IO]]
        val snapshot                   =
          FinancialSnapshot("vintage", AAPL, "Finnhub", ts, Map.empty, Nil, Nil, "{}", "hash", InvestmentScorer.MappingVersion, Nil)
        val error = new RuntimeException("History unavailable")
        when(client.getFinancialData(AAPL)).thenReturnIO(Some(FinancialData(AAPLFinancialMetrics, snapshot)))
        when(history.save(snapshot)).thenRaiseError(error)
        val res = for
          svc <- FinancialMetricsService.make(repo, client, dispatcher, Some(history))
          _   <- svc.fetchLatest(NonEmptyList.of(AAPL))
        yield ()
        res.attempt.asserting { result =>
          result mustBe Left(error)
          verifyNoInteractions(repo, dispatcher)
          succeed
        }
      }

      "fetch financial metrics, save them and dispatch Action.RecordFinancialMetricsUpdate" in {
        val (repo, client, dispatcher) = mocks
        when(client.getFinancialMetrics(any[Ticker])).thenReturnIO(Some(AAPLFinancialMetrics))
        when(repo.save(anyList[FinancialMetrics])).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit

        val res = for
          svc <- FinancialMetricsService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(AAPL))
        yield ()

        res.asserting { _ =>
          verify(client).getFinancialMetrics(AAPL)
          verify(repo).save(anyList[FinancialMetrics])
          verify(dispatcher).dispatch(Action.RecordFinancialMetricsUpdate(List(AAPL)))
          succeed
        }
      }

      "report errors when fetching financial metrics fails" in {
        val (repo, client, dispatcher) = mocks
        val error                      = new RuntimeException("API error")
        when(client.getFinancialMetrics(any[Ticker])).thenRaiseError(error)

        val res = for
          svc <- FinancialMetricsService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(AAPL))
        yield ()

        res.attempt.asserting { result =>
          verify(client).getFinancialMetrics(AAPL)
          verifyNoInteractions(repo, dispatcher)
          result mustBe Left(error)
        }
      }

      "persist each ticker before fetching the next" in {
        val (repo, client, dispatcher) = mocks
        val msftMetrics                = AAPLFinancialMetrics.copy(ticker = MSFT)
        when(client.getFinancialMetrics(AAPL)).thenReturnIO(Some(AAPLFinancialMetrics))
        when(client.getFinancialMetrics(MSFT)).thenReturnIO(Some(msftMetrics))
        when(repo.save(anyList[FinancialMetrics])).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit

        val res = for
          svc <- FinancialMetricsService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(AAPL, MSFT))
        yield ()

        res.asserting { _ =>
          verify(client).getFinancialMetrics(AAPL)
          verify(client).getFinancialMetrics(MSFT)
          verify(repo).save(List(AAPLFinancialMetrics))
          verify(repo).save(List(msftMetrics))
          verify(dispatcher).dispatch(Action.RecordFinancialMetricsUpdate(List(AAPL)))
          verify(dispatcher).dispatch(Action.RecordFinancialMetricsUpdate(List(MSFT)))
          succeed
        }
      }

      "skip tickers that return None from client" in {
        val (repo, client, dispatcher) = mocks
        when(client.getFinancialMetrics(AAPL)).thenReturnIO(Some(AAPLFinancialMetrics))
        when(client.getFinancialMetrics(MSFT)).thenReturnIO(None)
        when(repo.save(anyList[FinancialMetrics])).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit

        val res = for
          svc <- FinancialMetricsService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(AAPL, MSFT))
        yield ()

        res.asserting { _ =>
          verify(client).getFinancialMetrics(AAPL)
          verify(client).getFinancialMetrics(MSFT)
          verify(repo).save(anyList[FinancialMetrics])
          verify(dispatcher).dispatch(Action.RecordFinancialMetricsUpdate(List(AAPL)))
          succeed
        }
      }

      "handle errors when saving to repository fails" in {
        val (repo, client, dispatcher) = mocks
        val error                      = new RuntimeException("Database error")
        when(client.getFinancialMetrics(any[Ticker])).thenReturnIO(Some(AAPLFinancialMetrics))
        when(repo.save(anyList[FinancialMetrics])).thenRaiseError(error)

        val res = for
          svc <- FinancialMetricsService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(AAPL))
        yield ()

        res.attempt.asserting { result =>
          verify(client).getFinancialMetrics(AAPL)
          verify(repo).save(anyList[FinancialMetrics])
          result mustBe Left(error)
        }
      }
    }

    "find" should {
      "return financial metrics from repository when found and fetch is false" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnIO(Some(AAPLFinancialMetrics))

        val res = for
          svc    <- FinancialMetricsService.make(repo, client, dispatcher)
          result <- svc.find(AAPL, fetch = false)
        yield result

        res.asserting { result =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client, dispatcher)
          result mustBe AAPLFinancialMetrics
        }
      }

      "fetch from market data client when fetch is true" in {
        val (repo, client, dispatcher) = mocks
        when(client.getFinancialMetrics(any[Ticker])).thenReturnIO(Some(AAPLFinancialMetrics))
        when(repo.save(anyList[FinancialMetrics])).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc    <- FinancialMetricsService.make(repo, client, dispatcher)
          result <- svc.find(AAPL, fetch = true)
        yield result

        res.asserting { result =>
          verify(client).getFinancialMetrics(AAPL)
          verify(repo).save(anyList[FinancialMetrics])
          verify(dispatcher).dispatch(Action.RecordFinancialMetricsUpdate(List(AAPL)))
          result.ticker.mustBe(AAPL)
        }
      }

      "raise error when not found in repository and fetch is false" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnIO(None)

        val res = for
          svc    <- FinancialMetricsService.make(repo, client, dispatcher)
          result <- svc.find(AAPL, fetch = false)
        yield result

        res.attempt.asserting { result =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client, dispatcher)
          result.isLeft mustBe true
        }
      }

      "raise error when client returns None with fetch true" in {
        val (repo, client, dispatcher) = mocks
        when(client.getFinancialMetrics(any[Ticker])).thenReturnIO(None)

        val res = for
          svc    <- FinancialMetricsService.make(repo, client, dispatcher)
          result <- svc.find(AAPL, fetch = true)
        yield result

        res.attempt.asserting { result =>
          verify(client).getFinancialMetrics(AAPL)
          verifyNoInteractions(repo, dispatcher)
          result.isLeft mustBe true
        }
      }

      "handle errors when market data client fails with fetch true" in {
        val (repo, client, dispatcher) = mocks
        val error                      = new RuntimeException("API error")
        when(client.getFinancialMetrics(any[Ticker])).thenRaiseError(error)

        val res = for
          svc    <- FinancialMetricsService.make(repo, client, dispatcher)
          result <- svc.find(AAPL, fetch = true)
        yield result

        res.attempt.asserting { result =>
          verify(client).getFinancialMetrics(AAPL)
          result mustBe Left(error)
        }
      }
    }
  }

  def mocks: (FinancialMetricsRepository[IO], MarketDataClient[IO], ActionDispatcher[IO]) =
    (mock[FinancialMetricsRepository[IO]], mock[MarketDataClient[IO]], mock[ActionDispatcher[IO]])
}
