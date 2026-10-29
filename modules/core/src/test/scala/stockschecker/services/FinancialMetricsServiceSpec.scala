package stockschecker.services

import stockschecker.domain.{Provider, ProviderDeferralReason}
import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.actions.{Action, ActionDispatcher}
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{FinancialData, FinancialMetrics, FinancialSnapshot}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{FinancialHistoryRepository, FinancialMetricsRepository}
import stockschecker.fixtures.*

class FinancialMetricsServiceSpec extends IOWordSpec {
  private val snapshot =
    FinancialSnapshot(
      "vintage",
      AAPL,
      Provider.Finnhub,
      ts,
      Map.empty,
      Nil,
      Nil,
      "hash",
      "canonical-test-v1",
      Nil
    )

  "FinancialMetricsService" should {
    "persist financial evidence before publishing metrics and dispatch the update event" in {
      val (repo, client, dispatcher, history) = mocks
      when(client.getFinancialData(AAPL)).thenReturnSome(FinancialData(AAPLFinancialMetrics, snapshot))
      when(history.save(snapshot)).thenReturnUnit
      when(repo.save(anyList[FinancialMetrics])).thenReturnUnit
      when(dispatcher.dispatch(any[Action])).thenReturnUnit
      (for
        service <- FinancialMetricsService.make(repo, client, dispatcher, history)
        result  <- service.find(AAPL, true)
      yield result).asserting { result =>
        result mustBe AAPLFinancialMetrics
        verify(history).save(snapshot)
        verify(repo).save(List(AAPLFinancialMetrics))
        verify(dispatcher).dispatch(Action.RecordFinancialMetricsUpdate(NonEmptyList.one(AAPL)))
        succeed
      }
    }

    "not publish metrics when financial evidence cannot be saved" in {
      val (repo, client, dispatcher, history) = mocks
      val error                               = new RuntimeException("History unavailable")
      when(client.getFinancialData(AAPL)).thenReturnSome(FinancialData(AAPLFinancialMetrics, snapshot))
      when(history.save(snapshot)).thenRaiseError(error)
      (for
        service <- FinancialMetricsService.make(repo, client, dispatcher, history)
        result  <- service.find(AAPL, true)
      yield result).attempt.asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(repo, dispatcher)
        succeed
      }
    }

    "propagate provider deferrals without recording an update" in {
      val (repo, client, dispatcher, history) = mocks
      val deferred = AppError.ProviderDeferred(Provider.Finnhub, ts.plusSeconds(60), ProviderDeferralReason.Provider429)
      when(client.getFinancialData(AAPL)).thenRaiseError(deferred)
      (for
        service <- FinancialMetricsService.make(repo, client, dispatcher, history)
        result  <- service.find(AAPL, true)
      yield result).attempt.asserting { result =>
        result mustBe Left(deferred)
        verifyNoInteractions(repo, dispatcher, history)
        succeed
      }
    }

    "read cached metrics without contacting the provider" in {
      val (repo, client, dispatcher, history) = mocks
      when(repo.find(AAPL)).thenReturnSome(AAPLFinancialMetrics)
      (for
        service <- FinancialMetricsService.make(repo, client, dispatcher, history)
        result  <- service.find(AAPL, false)
      yield result).asserting { result =>
        result mustBe AAPLFinancialMetrics
        verifyNoInteractions(client, dispatcher, history)
        succeed
      }
    }

    "report an unavailable financial dataset without writing empty metrics" in {
      val (repo, client, dispatcher, history) = mocks
      when(client.getFinancialData(AAPL)).thenReturnNone
      (for
        service <- FinancialMetricsService.make(repo, client, dispatcher, history)
        result  <- service.find(AAPL, true)
      yield result).attempt.asserting { result =>
        result mustBe Left(AppError.FinancialMetricsNotFound(AAPL))
        verifyNoInteractions(repo, dispatcher, history)
        succeed
      }
    }
  }

  private def mocks = (
    mock[FinancialMetricsRepository[IO]],
    mock[MarketDataClient[IO]],
    mock[ActionDispatcher[IO]],
    mock[FinancialHistoryRepository[IO]]
  )
}
