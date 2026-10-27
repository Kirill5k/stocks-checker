package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{FinancialData, FinancialMetrics, FinancialSnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{CompanyProfileRepository, FinancialHistoryRepository, FinancialMetricsRepository}
import stockschecker.fixtures.*

class FinancialMetricsServiceSpec extends IOWordSpec {
  private val snapshot =
    FinancialSnapshot("vintage", AAPL, "Finnhub", ts, Map.empty, Nil, Nil, "{}", "hash", FinancialSnapshot.FinnhubMappingVersion, Nil)

  "FinancialMetricsService" should {
    "persist financial evidence before publishing metrics and record the update directly" in {
      val (repo, client, profiles, history) = mocks
      when(client.getFinancialData(AAPL)).thenReturnSome(FinancialData(AAPLFinancialMetrics, snapshot))
      when(history.save(snapshot)).thenReturnUnit
      when(repo.save(anyList[FinancialMetrics])).thenReturnUnit
      when(profiles.updateFinancialMetricsLastUpdated(anyList[Ticker])).thenReturnUnit
      (for
        service <- FinancialMetricsService.make(repo, client, profiles, history)
        result  <- service.find(AAPL, true)
      yield result).asserting { result =>
        result mustBe AAPLFinancialMetrics
        verify(history).save(snapshot)
        verify(repo).save(List(AAPLFinancialMetrics))
        verify(profiles).updateFinancialMetricsLastUpdated(List(AAPL))
        succeed
      }
    }

    "not publish metrics when financial evidence cannot be saved" in {
      val (repo, client, profiles, history) = mocks
      val error                             = new RuntimeException("History unavailable")
      when(client.getFinancialData(AAPL)).thenReturnSome(FinancialData(AAPLFinancialMetrics, snapshot))
      when(history.save(snapshot)).thenRaiseError(error)
      (for
        service <- FinancialMetricsService.make(repo, client, profiles, history)
        result  <- service.find(AAPL, true)
      yield result).attempt.asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(repo, profiles)
        succeed
      }
    }

    "propagate provider deferrals without recording an update" in {
      val (repo, client, profiles, history) = mocks
      val deferred                          = AppError.ProviderDeferred("Finnhub", ts.plusSeconds(60), "rate-limited")
      when(client.getFinancialData(AAPL)).thenRaiseError(deferred)
      (for
        service <- FinancialMetricsService.make(repo, client, profiles, history)
        result  <- service.find(AAPL, true)
      yield result).attempt.asserting { result =>
        result mustBe Left(deferred)
        verifyNoInteractions(repo, profiles, history)
        succeed
      }
    }

    "read cached metrics without contacting the provider" in {
      val (repo, client, profiles, history) = mocks
      when(repo.find(AAPL)).thenReturnSome(AAPLFinancialMetrics)
      (for
        service <- FinancialMetricsService.make(repo, client, profiles, history)
        result  <- service.find(AAPL, false)
      yield result).asserting { result =>
        result mustBe AAPLFinancialMetrics
        verifyNoInteractions(client, profiles, history)
        succeed
      }
    }

    "report an unavailable financial dataset without writing empty metrics" in {
      val (repo, client, profiles, history) = mocks
      when(client.getFinancialData(AAPL)).thenReturnNone
      (for
        service <- FinancialMetricsService.make(repo, client, profiles, history)
        result  <- service.find(AAPL, true)
      yield result).attempt.asserting { result =>
        result mustBe Left(AppError.FinancialMetricsNotFound(AAPL))
        verifyNoInteractions(repo, profiles, history)
        succeed
      }
    }
  }

  private def mocks = (
    mock[FinancialMetricsRepository[IO]],
    mock[MarketDataClient[IO]],
    mock[CompanyProfileRepository[IO]],
    mock[FinancialHistoryRepository[IO]]
  )
}
