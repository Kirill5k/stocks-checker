package stockschecker.services

import stockschecker.domain.Provider
import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{DateRange, PriceAdjustment, PriceCandle, PriceHistorySnapshot, Ticker}
import stockschecker.repositories.PriceHistoryRepository

import java.time.{Instant, LocalDate, ZoneOffset}

class PriceHistoryServiceSpec extends IOWordSpec {
  private val now             = Instant.parse("2026-10-27T12:00:00Z")
  private val today           = LocalDate.parse("2026-10-27")
  private val ticker          = Ticker("AAA")
  private val range           = DateRange(today.minusYears(13), today)
  private val historicalRange = DateRange(today.minusYears(1), today.minusDays(2))
  private val snapshot        = PriceHistorySnapshot(
    "cached",
    ticker,
    List(PriceCandle(today, 100, 100, 100, 100, 1L)),
    Provider.TwelveData,
    "USD",
    PriceAdjustment.SplitsAndDividends,
    now,
    range
  )
  given Clock[IO] = Clock.mock(now)

  private class Dependencies {
    val repo    = mock[PriceHistoryRepository[IO]]
    val client  = mock[MarketDataClient[IO]]
    val fetched = snapshot.copy(id = "fetched")
    when(repo.save(any[PriceHistorySnapshot])).thenReturnUnit
    when(client.getDailyPriceHistory(ticker, range, PriceAdjustment.SplitsAndDividends)).thenReturnIO(fetched)

    def service: IO[PriceHistoryService[IO]] = PriceHistoryService.make(client, repo, DataQualityServiceStub)
  }

  "Price history acquisition" should {
    List(
      "at the captured instant"          -> (now, true),
      "just inside the freshness window" -> (now.minusSeconds(86400).plusNanos(1), true),
      "exactly one day old"              -> (now.minusSeconds(86400), false),
      "in the future"                    -> (now.plusNanos(1), false)
    ).foreach { case (description, (fetchedAt, reusable)) =>
      s"apply the recent observation boundary when its timestamp is $description" in {
        val d      = new Dependencies
        val stored = snapshot.copy(fetchedAt = fetchedAt)
        when(d.repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, range)).thenReturnIO(Some(stored))
        d.service.flatMap(_.recent(ticker, range, PriceAdjustment.SplitsAndDividends, now)).asserting { result =>
          result mustBe (if (reusable) stored else d.fetched)
          if (reusable) {
            verifyNoInteractions(d.client)
            verify(d.repo, never).save(any[PriceHistorySnapshot])
          } else {
            verify(d.client).getDailyPriceHistory(ticker, range, PriceAdjustment.SplitsAndDividends)
            verify(d.repo).save(d.fetched)
          }
          succeed
        }
      }
    }

    "select the latest unadjusted observation and refetch when it does not cover the quote range" in {
      val d          = new Dependencies
      val quoteRange = DateRange(today.minusDays(14), today)
      val quoted     = snapshot.copy(id = "quote", adjustment = PriceAdjustment.Unadjusted, requestedRange = quoteRange)
      when(d.repo.latest(ticker, PriceAdjustment.Unadjusted))
        .thenReturnIO(Some(quoted.copy(requestedRange = DateRange(today.minusDays(1), today))))
      when(d.client.getDailyPriceHistory(ticker, quoteRange, PriceAdjustment.Unadjusted)).thenReturnIO(quoted)
      d.service.flatMap(_.recent(ticker, quoteRange, PriceAdjustment.Unadjusted, now)).asserting { result =>
        result mustBe quoted
        verify(d.repo).latest(ticker, PriceAdjustment.Unadjusted)
        verify(d.repo, never).latestCovering(any[Ticker], any[PriceAdjustment], any[DateRange])
        verify(d.repo).save(quoted)
        succeed
      }
    }

    List(
      "different ticker"     -> snapshot.copy(ticker = Ticker("BBB")),
      "different adjustment" -> snapshot.copy(adjustment = PriceAdjustment.Splits),
      "blank currency"       -> snapshot.copy(currency = " ")
    ).foreach { case (description, invalid) =>
      s"refetch a recent cached observation with a $description" in {
        val d = new Dependencies
        when(d.repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, range)).thenReturnIO(Some(invalid))
        d.service.flatMap(_.recent(ticker, range, PriceAdjustment.SplitsAndDividends, now)).asserting { result =>
          result mustBe d.fetched
          verify(d.repo).save(d.fetched)
          succeed
        }
      }
    }

    List("before" -> -1L, "at" -> 0L).foreach { case (description, nanos) =>
      s"check completed sessions $description the next UTC midnight without imposing a cache age limit" in {
        val d           = new Dependencies
        val completedAt = historicalRange.through.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        val stored      = snapshot.copy(fetchedAt = completedAt.plusNanos(nanos))
        when(d.repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, historicalRange)).thenReturnIO(Some(stored))
        d.service.flatMap(_.cachedHistorical(ticker, historicalRange)).asserting { result =>
          result mustBe (if (nanos == 0L) Some(stored) else None)
          verifyNoInteractions(d.client)
          verify(d.repo, never).save(any[PriceHistorySnapshot])
          succeed
        }
      }
    }

    List(
      "just inside the freshness window" -> (now.minusSeconds(86400).plusNanos(1), true),
      "exactly one day old"              -> (now.minusSeconds(86400), false),
      "in the future"                    -> (now.plusNanos(1), false)
    ).foreach { case (description, (fetchedAt, reusable)) =>
      s"apply freshness to explicit historical capture when its timestamp is $description" in {
        val d      = new Dependencies
        val stored = snapshot.copy(fetchedAt = fetchedAt)
        when(d.repo.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, historicalRange)).thenReturnIO(Some(stored))
        d.service.flatMap(_.historical(ticker, historicalRange)).asserting { result =>
          result mustBe (if (reusable) stored else d.fetched)
          if (reusable) verifyNoInteractions(d.client)
          else {
            verify(d.client).getDailyPriceHistory(ticker, range, PriceAdjustment.SplitsAndDividends)
            verify(d.repo).save(d.fetched)
          }
          succeed
        }
      }
    }
  }
}
