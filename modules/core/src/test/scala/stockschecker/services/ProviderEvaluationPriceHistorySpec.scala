package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.PriceHistoryRepository
import java.time.{Instant, LocalDate}

class ProviderEvaluationPriceHistorySpec extends IOWordSpec {
  private val ticker = Ticker("AAA")
  private val from = LocalDate.parse("2025-01-01")
  private val through = LocalDate.parse("2025-02-01")
  private val bar = PriceCandle(from.plusDays(1), 100, 100, 100, 100, 1L)
  private val snapshot = PriceHistorySnapshot("price-vintage-1", ticker, List(bar), "TwelveData", "USD", "split-and-dividend",
    Instant.parse("2025-02-02T12:00:00Z"), from, through)

  "Evaluation price history" should {
    "reuse a complete archived vintage and include its immutable reference" in {
      val repo = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(snapshot))
      val history = ProviderEvaluationPriceHistory.make(client, repo, audited = true)
      history.history(ticker, from, through).asserting { result =>
        result.snapshotIds mustBe List(snapshot.id)
        result.fetchedAt mustBe snapshot.fetchedAt
        verifyNoInteractions(client)
        succeed
      }
    }
    "leave an incomplete vintage available in storage but require a new capture for a later cutoff" in {
      val repo = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(snapshot))
      val later = through.plusMonths(1)
      val failure = AppError.HttpClient("TwelveData", 404, "unavailable")
      when(client.getDailyPriceHistory(ticker, from, later, "all")).thenRaiseError(failure)
      val history = ProviderEvaluationPriceHistory.make(client, repo, audited = true)
      (for
        cache <- history.cached(ticker, from, later)
        refresh <- history.history(ticker, from, later).attempt
      yield (cache, refresh)).asserting { case (cache, refresh) =>
        cache mustBe None
        refresh mustBe Left(failure)
        verify(repo, never).save(any[PriceHistorySnapshot])
        succeed
      }
    }
    "disclose unaudited total return instead of claiming valid investment outcomes" in {
      val repo = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(snapshot))
      ProviderEvaluationPriceHistory.make(client, repo, audited = false).cached(ticker, from, through).asserting { result =>
        result.get.audited mustBe false
        result.get.issues must contain("total-return provider contract has not been audited")
      }
    }
  }
}
