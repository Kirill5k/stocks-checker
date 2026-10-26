package stockschecker.services

import cats.effect.IO
import cats.syntax.all.*
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.PriceHistoryRepository
import java.time.{Instant, LocalDate, ZoneOffset}

class EvaluationPriceHistorySpec extends IOWordSpec {
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
    PriceAdjustment.SplitsAndDividends,
    Instant.parse("2025-02-02T12:00:00Z"),
    DateRange(from, through)
  )

  "Evaluation price history" should {
    "reuse a retained complete vintage and include its provenance ID" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from)).thenReturnIO(Some(snapshot))
      val history = EvaluationPriceHistory.make(client, repo)
      history.cached(ticker, from, through).asserting { result =>
        result.get.snapshotIds mustBe List(snapshot.id)
        result.get.fetchedAt mustBe snapshot.fetchedAt
        result.get.audited mustBe true
        result.get.issues mustBe Nil
        verifyNoInteractions(client)
        succeed
      }
    }

    "refresh an old vintage on explicit retry and share the full analytics range across screens" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from)).thenReturnIO(Some(snapshot))
      when(repo.save(any[PriceHistorySnapshot])).thenReturnUnit
      (for
        now <- IO.realTimeInstant
        today      = LocalDate.ofInstant(now, ZoneOffset.UTC)
        sharedFrom = today.minusYears(13)
        refreshed  = snapshot.copy(id = "refreshed", fetchedAt = now, requestedRange = DateRange(sharedFrom, today))
        _          = when(client.getDailyPriceHistory(ticker, refreshed.requestedRange, PriceAdjustment.SplitsAndDividends)).thenReturnIO(
          refreshed
        )
        history = EvaluationPriceHistory.make(client, repo)
        result <- history.history(ticker, from, through)
      yield (result, refreshed)).asserting { case (result, refreshed) =>
        result.snapshotIds mustBe List("refreshed")
        result.closes mustBe List(EvaluationClose(bar.date, bar.close))
        verify(repo).save(refreshed)
        verify(client).getDailyPriceHistory(
          ticker,
          refreshed.requestedRange,
          PriceAdjustment.SplitsAndDividends
        )
        succeed
      }
    }

    "reuse a fresh broad vintage for multiple screen dates without extra requests" in {
      val repo   = mock[PriceHistoryRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      (for
        now <- IO.realTimeInstant
        today   = LocalDate.ofInstant(now, ZoneOffset.UTC)
        shared  = snapshot.copy(fetchedAt = now, requestedRange = DateRange(today.minusYears(13), today))
        _       = when(repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from)).thenReturnIO(Some(shared))
        _       = when(repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from.plusDays(10))).thenReturnIO(Some(shared))
        history = EvaluationPriceHistory.make(client, repo)
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
      when(repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from)).thenReturnIO(Some(snapshot))
      val later   = through.plusMonths(1)
      val failure = AppError.HttpClient("TwelveData", 404, "unavailable")
      when(client.getDailyPriceHistory(any[Ticker], any[DateRange], any[PriceAdjustment])).thenRaiseError(failure)
      val history = EvaluationPriceHistory.make(client, repo)
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

    "exclude unsupported currencies, missing total-return adjustments and possibly truncated vintages from investment returns" in {
      val truncated = snapshot.copy(
        candles = (0 until 5000).toList.map(n => bar.copy(date = through.minusDays(n.toLong))),
        requestedRange = DateRange(through.minusDays(4999), through)
      )
      List(
        snapshot.copy(currency = "EUR")                    -> "unsupported evaluation currency",
        snapshot.copy(adjustment = PriceAdjustment.Splits) -> "total-return adjustments unavailable",
        truncated                                          -> "provider response may be truncated"
      ).traverse_ { case (invalid, issue) =>
        val repo   = mock[PriceHistoryRepository[IO]]
        val client = mock[MarketDataClient[IO]]
        when(repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from)).thenReturnIO(Some(invalid))
        EvaluationPriceHistory.make(client, repo).cached(ticker, from, through).map { result =>
          val series = result.get
          series.audited mustBe false
          series.issues must contain(issue)
          val report = InvestmentEvaluator.evaluate(
            InvestmentScreenId("invalid-prices"),
            from.atStartOfDay(ZoneOffset.UTC).toInstant,
            snapshot.fetchedAt,
            List(InvestmentCohort("investment", List(ticker))),
            Map(ticker -> series, InvestmentEvaluator.Benchmark -> series.copy(ticker = InvestmentEvaluator.Benchmark)),
            Some(through)
          )
          report.horizons.flatMap(_.cohorts).foreach { cohort =>
            cohort.status mustBe "incomplete"
            cohort.totalReturnPercent mustBe None
            cohort.observedReturnPercent mustBe None
            cohort.issues.mkString must include(issue)
          }
          verifyNoInteractions(client)
        }
      }.asserting(_ => succeed)
    }
  }
}
