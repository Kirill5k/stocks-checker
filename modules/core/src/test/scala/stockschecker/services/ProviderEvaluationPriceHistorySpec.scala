package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.PriceHistoryRepository
import java.time.{Instant, LocalDate, ZoneOffset}

class ProviderEvaluationPriceHistorySpec extends IOWordSpec {
  private val ticker   = Ticker("AAA")
  private val from     = LocalDate.parse("2025-01-01")
  private val through  = LocalDate.parse("2025-02-01")
  private val bar      = PriceCandle(from.plusDays(1), 100, 100, 100, 100, 1L)
  private val snapshot = PriceHistorySnapshot(
    "price-vintage-1",
    ticker,
    List(bar),
    "TwelveData",
    "USD",
    "split-and-dividend",
    Instant.parse("2025-02-02T12:00:00Z"),
    from,
    through
  )

  "Evaluation price history" should {
    "reuse a retained complete vintage and include its provenance ID" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(snapshot))
      val history = ProviderEvaluationPriceHistory.make(client, repo, audited = true)
      history.cached(ticker, from, through).asserting { result =>
        result.get.snapshotIds mustBe List(snapshot.id)
        result.get.fetchedAt mustBe snapshot.fetchedAt
        verifyNoInteractions(client)
        succeed
      }
    }

    "refresh an old vintage on explicit retry and share the full analytics range across screens" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(snapshot))
      when(repo.save(any[PriceHistorySnapshot])).thenReturnUnit
      (for
        now <- IO.realTimeInstant
        today      = LocalDate.ofInstant(now, ZoneOffset.UTC)
        sharedFrom = today.minusYears(13)
        refreshed  = snapshot.copy(id = "refreshed", fetchedAt = now, requestedFrom = sharedFrom, requestedThrough = today)
        _          = when(client.getDailyPriceHistory(ticker, sharedFrom, today, "all")).thenReturnIO(refreshed)
        history    = ProviderEvaluationPriceHistory.make(client, repo, audited = true)
        result <- history.history(ticker, from, through)
      yield (result, refreshed)).asserting { case (result, refreshed) =>
        result.snapshotIds mustBe List("refreshed")
        result.closes mustBe List(EvaluationClose(bar.date, bar.close))
        verify(repo).save(refreshed)
        verify(client).getDailyPriceHistory(ticker, refreshed.requestedFrom, refreshed.requestedThrough, "all")
        succeed
      }
    }

    "reuse a fresh broad vintage for multiple screen dates without extra requests" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      (for
        now <- IO.realTimeInstant
        today   = LocalDate.ofInstant(now, ZoneOffset.UTC)
        shared  = snapshot.copy(fetchedAt = now, requestedFrom = today.minusYears(13), requestedThrough = today)
        _       = when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(shared))
        _       = when(repo.latestCovering(ticker, "split-and-dividend", from.plusDays(10))).thenReturnIO(Some(shared))
        history = ProviderEvaluationPriceHistory.make(client, repo, audited = true)
        first  <- history.history(ticker, from, through)
        second <- history.history(ticker, from.plusDays(10), through)
      yield (first, second)).asserting { case (first, second) =>
        first.snapshotIds mustBe second.snapshotIds
        verifyNoInteractions(client)
        succeed
      }
    }

    "keep an incomplete vintage available when a subsequent refresh fails" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(snapshot))
      val later   = through.plusMonths(1)
      val failure = AppError.HttpClient("TwelveData", 404, "unavailable")
      when(client.getDailyPriceHistory(any[Ticker], any[LocalDate], any[LocalDate], anyString)).thenRaiseError(failure)
      val history = ProviderEvaluationPriceHistory.make(client, repo, audited = true)
      (for
        cache   <- history.cached(ticker, from, later)
        refresh <- history.history(ticker, from, later).attempt
      yield (cache, refresh)).asserting { case (cache, refresh) =>
        cache mustBe None
        refresh mustBe Left(failure)
        verify(repo, never).save(any[PriceHistorySnapshot])
        succeed
      }
    }

    "disclose unaudited total return instead of claiming valid investment outcomes" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, "split-and-dividend", from)).thenReturnIO(Some(snapshot))
      ProviderEvaluationPriceHistory.make(client, repo, audited = false).cached(ticker, from, through).asserting { result =>
        result.get.audited mustBe false
        result.get.issues must contain("total-return provider contract has not been audited")
      }
    }
  }
}
