package stockschecker.services

import stockschecker.domain.{Provider, ProviderDeferralReason}
import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.actions.{Action, ActionDispatcher}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{DateRange, PriceAdjustment, PriceAnalytics, PriceCandle, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{PriceAnalyticsRepository, PriceHistoryRepository}
import stockschecker.fixtures.*
import java.time.{Instant, LocalDate, ZoneOffset}

class PriceServiceSpec extends IOWordSpec {
  private val fixedNow = Instant.parse("2026-10-27T12:00:00Z")
  given Clock[IO]      = Clock.mock(fixedNow)

  private case class Dependencies(
      repo: PriceAnalyticsRepository[IO],
      client: MarketDataClient[IO],
      dispatcher: ActionDispatcher[IO],
      history: PriceHistoryRepository[IO]
  ) {
    def service: IO[PriceService[IO]] =
      PriceService.make(repo, dispatcher, history, client)
    def cache(adjusted: PriceHistorySnapshot, quoted: PriceHistorySnapshot): Unit = {
      val range = DateRange(adjusted.requestedRange.through.minusYears(13), adjusted.requestedRange.through)
      when(history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, range))
        .thenReturnIO(Some(adjusted))
      when(history.latest(AAPL, PriceAdjustment.Unadjusted)).thenReturnIO(Some(quoted))
      ()
    }
  }
  private def mocks: Dependencies = {
    val d = Dependencies(
      mock[PriceAnalyticsRepository[IO]],
      mock[MarketDataClient[IO]],
      mock[ActionDispatcher[IO]],
      mock[PriceHistoryRepository[IO]]
    )
    when(d.repo.save(anyList[PriceAnalytics])).thenReturnUnit
    when(d.dispatcher.dispatch(any[Action])).thenReturnUnit
    when(d.history.save(any[PriceHistorySnapshot])).thenReturnUnit
    d
  }
  private def snapshot(now: Instant, adjustment: PriceAdjustment, offsets: List[Long] = List(0L)): PriceHistorySnapshot = {
    val through = LocalDate.ofInstant(now, ZoneOffset.UTC)
    val close   = if (adjustment == PriceAdjustment.Unadjusted) BigDecimal(100) else BigDecimal(50)
    PriceHistorySnapshot(
      adjustment.value,
      AAPL,
      offsets.map(offset => PriceCandle(through.minusDays(offset), close, close, close, close, 1L)),
      Provider.TwelveData,
      "USD",
      adjustment,
      now,
      DateRange(through.minusYears(13), through)
    )
  }

  "PriceService" should {
    List(
      "at the captured instant"          -> (fixedNow, true),
      "just inside the freshness window" -> (fixedNow.minusSeconds(86400).plusNanos(1), true),
      "exactly one day old"              -> (fixedNow.minusSeconds(86400), false),
      "in the future"                    -> (fixedNow.plusNanos(1), false)
    ).foreach { case (description, (fetchedAt, reusable)) =>
      s"apply the recent observation boundary when its timestamp is $description" in {
        val d    = mocks
        val full = snapshot(fixedNow, PriceAdjustment.SplitsAndDividends)
        d.cache(full.copy(fetchedAt = fetchedAt), snapshot(fixedNow, PriceAdjustment.Unadjusted))
        when(d.client.getDailyPriceHistory(AAPL, full.requestedRange, PriceAdjustment.SplitsAndDividends)).thenReturnIO(full)
        d.service.flatMap(_.findPriceAnalytics(AAPL, fetch = true)).asserting { _ =>
          if reusable then verifyNoInteractions(d.client)
          else verify(d.client).getDailyPriceHistory(AAPL, full.requestedRange, PriceAdjustment.SplitsAndDividends)
          verify(d.history, never).save(any[PriceHistorySnapshot])
          succeed
        }
      }
    }

    List(
      "different ticker"     -> snapshot(fixedNow, PriceAdjustment.SplitsAndDividends).copy(ticker = MSFT),
      "different adjustment" -> snapshot(fixedNow, PriceAdjustment.Splits),
      "blank currency"       -> snapshot(fixedNow, PriceAdjustment.SplitsAndDividends).copy(currency = " ")
    ).foreach { case (description, invalid) =>
      s"refetch a recent cached observation with a $description" in {
        val d    = mocks
        val full = snapshot(fixedNow, PriceAdjustment.SplitsAndDividends)
        d.cache(invalid, snapshot(fixedNow, PriceAdjustment.Unadjusted))
        when(d.client.getDailyPriceHistory(AAPL, full.requestedRange, PriceAdjustment.SplitsAndDividends)).thenReturnIO(full)
        d.service.flatMap(_.findPriceAnalytics(AAPL, fetch = true)).asserting { _ =>
          verify(d.client).getDailyPriceHistory(AAPL, full.requestedRange, PriceAdjustment.SplitsAndDividends)
          verify(d.history, never).save(any[PriceHistorySnapshot])
          succeed
        }
      }
    }

    "refetch and cache the latest unadjusted observation when it does not cover the quote range" in {
      val d      = mocks
      val full   = snapshot(fixedNow, PriceAdjustment.SplitsAndDividends)
      val today  = full.requestedRange.through
      val quoted = snapshot(fixedNow, PriceAdjustment.Unadjusted).copy(requestedRange = DateRange(today.minusDays(14), today))
      d.cache(full, quoted.copy(requestedRange = DateRange(today.minusDays(1), today)))
      when(d.client.getDailyPriceHistory(AAPL, quoted.requestedRange, PriceAdjustment.Unadjusted)).thenReturnIO(quoted)
      d.service.flatMap(_.findPriceAnalytics(AAPL, fetch = true)).asserting { _ =>
        verify(d.history).latest(AAPL, PriceAdjustment.Unadjusted)
        verify(d.history).save(quoted)
        verify(d.client).getDailyPriceHistory(AAPL, quoted.requestedRange, PriceAdjustment.Unadjusted)
        succeed
      }
    }

    List(
      "just inside the freshness window" -> (fixedNow.minusSeconds(86400).plusNanos(1), true),
      "exactly one day old"              -> (fixedNow.minusSeconds(86400), false),
      "in the future"                    -> (fixedNow.plusNanos(1), false)
    ).foreach { case (description, (fetchedAt, reusable)) =>
      s"apply freshness to explicit historical capture when its timestamp is $description" in {
        val d       = mocks
        val today   = LocalDate.ofInstant(fixedNow, ZoneOffset.UTC)
        val through = today.minusDays(2)
        val range   = DateRange(through.minusYears(13), through)
        val shared  = DateRange(range.from, today)
        val fetched = snapshot(fixedNow, PriceAdjustment.SplitsAndDividends).copy(requestedRange = shared)
        when(d.history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, range))
          .thenReturnIO(Some(fetched.copy(fetchedAt = fetchedAt)))
        when(d.client.getDailyPriceHistory(AAPL, shared, PriceAdjustment.SplitsAndDividends)).thenReturnIO(fetched)
        d.service.flatMap(_.refreshHistory(AAPL, through)).asserting { _ =>
          if reusable then {
            verifyNoInteractions(d.client)
            verify(d.history, never).save(any[PriceHistorySnapshot])
          } else {
            verify(d.client).getDailyPriceHistory(AAPL, shared, PriceAdjustment.SplitsAndDividends)
            verify(d.history).save(fetched)
          }
          verifyNoInteractions(d.repo, d.dispatcher)
          succeed
        }
      }
    }

    "reuse a complete fresh broad vintage across capture dates without provider requests" in {
      val d       = mocks
      val today   = LocalDate.ofInstant(fixedNow, ZoneOffset.UTC)
      val through = today.minusDays(2)
      val stored  = snapshot(fixedNow, PriceAdjustment.SplitsAndDividends)
        .copy(requestedRange = DateRange(through.minusYears(13), today))
      when(d.history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, DateRange(through.minusYears(13), through)))
        .thenReturnIO(Some(stored))
      val later = through.plusDays(1)
      when(d.history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, DateRange(later.minusYears(13), later)))
        .thenReturnIO(Some(stored))
      (for
        service <- d.service
        _       <- service.refreshHistory(AAPL, through)
        _       <- service.refreshHistory(AAPL, later)
      yield ()).asserting { _ =>
        verifyNoInteractions(d.client, d.repo, d.dispatcher)
        verify(d.history, never).save(any[PriceHistorySnapshot])
        succeed
      }
    }

    "refetch historical observations that predate the last requested session's completion" in {
      val d       = mocks
      val today   = LocalDate.ofInstant(fixedNow, ZoneOffset.UTC)
      val through = today.minusDays(1)
      val range   = DateRange(through.minusYears(13), through)
      val shared  = DateRange(range.from, today)
      val stored  = snapshot(fixedNow, PriceAdjustment.SplitsAndDividends).copy(
        fetchedAt = today.atStartOfDay(ZoneOffset.UTC).toInstant.minusNanos(1),
        requestedRange = shared
      )
      val fetched = stored.copy(fetchedAt = fixedNow)
      when(d.history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, range)).thenReturnIO(Some(stored))
      when(d.client.getDailyPriceHistory(AAPL, shared, PriceAdjustment.SplitsAndDividends)).thenReturnIO(fetched)
      d.service.flatMap(_.refreshHistory(AAPL, through)).asserting { _ =>
        verify(d.history).save(fetched)
        verify(d.client).getDailyPriceHistory(AAPL, shared, PriceAdjustment.SplitsAndDividends)
        succeed
      }
    }

    "retain stored history when an explicit refresh fails" in {
      val d       = mocks
      val today   = LocalDate.ofInstant(fixedNow, ZoneOffset.UTC)
      val through = today.minusDays(2)
      val range   = DateRange(through.minusYears(13), through)
      val stored  = snapshot(fixedNow.minusSeconds(172800), PriceAdjustment.SplitsAndDividends)
      val failure = AppError.HttpClient(Provider.TwelveData.print, 404, "unavailable")
      when(d.history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, range)).thenReturnIO(Some(stored))
      when(d.client.getDailyPriceHistory(any[Ticker], any[DateRange], any[PriceAdjustment])).thenRaiseError(failure)
      d.service.flatMap(_.refreshHistory(AAPL, through)).attempt.asserting { result =>
        result mustBe Left(failure)
        verify(d.history, never).save(any[PriceHistorySnapshot])
        verifyNoInteractions(d.repo, d.dispatcher)
        succeed
      }
    }

    "reuse daily observations, save analytics and dispatch the profile update event" in {
      val d = mocks
      (for
        now <- IO.pure(fixedNow)
        _ = d.cache(snapshot(now, PriceAdjustment.SplitsAndDividends), snapshot(now, PriceAdjustment.Unadjusted))
        service   <- d.service
        analytics <- service.findPriceAnalytics(AAPL, fetch = true)
      yield analytics).asserting { analytics =>
        analytics.performanceSummary.latestPrice mustBe BigDecimal(100)
        analytics.basis.flatMap(_.priceObservationId) mustBe Some("split-and-dividend")
        analytics.basis.flatMap(_.quotedPriceObservationId) mustBe Some("none")
        val ordered = org.mockito.Mockito.inOrder(d.repo, d.dispatcher)
        ordered.verify(d.repo).save(List(analytics))
        ordered.verify(d.dispatcher).dispatch(Action.RecordPriceAnalyticsUpdate(NonEmptyList.one(AAPL)))
        verifyNoInteractions(d.client)
        succeed
      }
    }

    "align a cached adjusted series and a later quote to their latest common trading session" in {
      val d = mocks
      (for
        now <- IO.pure(fixedNow)
        _ = d.cache(
          snapshot(now, PriceAdjustment.SplitsAndDividends, List(1L, 2L)),
          snapshot(now, PriceAdjustment.Unadjusted, List(0L, 1L, 2L))
        )
        service   <- d.service
        analytics <- service.findPriceAnalytics(AAPL, fetch = true)
      yield (analytics, LocalDate.ofInstant(now, ZoneOffset.UTC))).asserting { case (analytics, today) =>
        analytics.performanceSummary.latestPriceDate mustBe today.minusDays(1)
        analytics.performanceSummary.latestPrice mustBe BigDecimal(100)
        verifyNoInteractions(d.client)
        succeed
      }
    }

    "report disjoint sessions as unavailable data eligible for bounded retries" in {
      val d = mocks
      (for
        now <- IO.pure(fixedNow)
        _ = d.cache(snapshot(now, PriceAdjustment.SplitsAndDividends, List(0L)), snapshot(now, PriceAdjustment.Unadjusted, List(1L)))
        service <- d.service
        outcome <- service.findPriceAnalytics(AAPL, fetch = true).attempt
      yield outcome).asserting { outcome =>
        outcome mustBe Left(
          AppError.DataUnavailable(AAPL, "adjusted and quoted histories have no common trading session")
        )
        verify(d.repo, never).save(anyList[PriceAnalytics])
        verifyNoInteractions(d.dispatcher)
        verifyNoInteractions(d.client)
        succeed
      }
    }

    "report a halted ticker's stale session as unavailable data without indefinitely deferring its job" in {
      val d = mocks
      (for
        now <- IO.pure(fixedNow)
        _ = d.cache(snapshot(now, PriceAdjustment.SplitsAndDividends, List(45L)), snapshot(now, PriceAdjustment.Unadjusted, List(45L)))
        service <- d.service
        outcome <- service.findPriceAnalytics(AAPL, fetch = true).attempt
      yield outcome).asserting { outcome =>
        outcome mustBe Left(
          AppError.DataUnavailable(AAPL, "the latest matching trading session is more than seven days old")
        )
        verify(d.repo, never).save(anyList[PriceAnalytics])
        verifyNoInteractions(d.dispatcher)
        verifyNoInteractions(d.client)
        succeed
      }
    }

    "reject mismatched currencies without publishing analytics" in {
      val d = mocks
      (for
        now <- IO.pure(fixedNow)
        _ = d.cache(snapshot(now, PriceAdjustment.SplitsAndDividends), snapshot(now, PriceAdjustment.Unadjusted).copy(currency = "CAD"))
        service <- d.service
        result  <- service.findPriceAnalytics(AAPL, fetch = true).attempt
      yield result).asserting { result =>
        result.left.toOption.get mustBe a[AppError.FailedValidation]
        verify(d.repo, never).save(anyList[PriceAnalytics])
        succeed
      }
    }

    "refetch fresh cached history when it does not cover the requested horizon" in {
      val d = mocks
      (for
        now <- IO.pure(fixedNow)
        full = snapshot(now, PriceAdjustment.SplitsAndDividends)
        _    = d.cache(
          full.copy(requestedRange = full.requestedRange.copy(from = full.requestedRange.through.minusDays(14))),
          snapshot(now, PriceAdjustment.Unadjusted)
        )
        _ = when(
          d.client.getDailyPriceHistory(AAPL, full.requestedRange, PriceAdjustment.SplitsAndDividends)
        ).thenReturnIO(full)
        service <- d.service
        _       <- service.findPriceAnalytics(AAPL, fetch = true)
      yield full).asserting { full =>
        verify(d.history, never).save(any[PriceHistorySnapshot])
        verify(d.client).getDailyPriceHistory(
          AAPL,
          full.requestedRange,
          PriceAdjustment.SplitsAndDividends
        )
        succeed
      }
    }

    "retain only the small quote when the adjusted history request is rate limited" in {
      val d = mocks
      (for
        now <- IO.pure(fixedNow)
        full       = snapshot(now, PriceAdjustment.SplitsAndDividends)
        quoteRange = DateRange(full.requestedRange.through.minusDays(14), full.requestedRange.through)
        quoted     = snapshot(now, PriceAdjustment.Unadjusted).copy(requestedRange = quoteRange)
        _          = when(d.history.latest(any[Ticker], any[PriceAdjustment])).thenReturnIO(None)
        _          = when(d.history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, full.requestedRange)).thenReturnIO(None)
        deferred   = AppError.ProviderDeferred(Provider.TwelveData, now.plusSeconds(60), ProviderDeferralReason.Provider429)
        _ = when(d.client.getDailyPriceHistory(AAPL, full.requestedRange, PriceAdjustment.SplitsAndDividends)).thenRaiseError(deferred)
        _ = when(
          d.client
            .getDailyPriceHistory(
              AAPL,
              quoteRange,
              PriceAdjustment.Unadjusted
            )
        )
          .thenReturnIO(quoted)
        service <- d.service
        result  <- service.findPriceAnalytics(AAPL, fetch = true).attempt
      yield (full, quoted, deferred, result)).asserting { case (full, quoted, deferred, result) =>
        result mustBe Left(deferred)
        val ordered = org.mockito.Mockito.inOrder(d.client, d.history)
        ordered
          .verify(d.client)
          .getDailyPriceHistory(
            AAPL,
            quoted.requestedRange,
            PriceAdjustment.Unadjusted
          )
        ordered.verify(d.history).save(quoted)
        ordered.verify(d.client).getDailyPriceHistory(AAPL, full.requestedRange, PriceAdjustment.SplitsAndDividends)
        verify(d.repo, never).save(anyList[PriceAnalytics])
        succeed
      }
    }

    "return stored analytics without consulting providers when fetch is false" in {
      val d = mocks
      when(d.repo.find(AAPL)).thenReturnIO(Some(AAPLPriceAnalytics))
      d.service.flatMap(_.findPriceAnalytics(AAPL, fetch = false)).asserting { result =>
        result mustBe AAPLPriceAnalytics
        verifyNoInteractions(d.client, d.history)
        succeed
      }
    }

    "report a missing stored analytics result" in {
      val d = mocks
      when(d.repo.find(AAPL)).thenReturnIO(None)
      d.service.flatMap(_.findPriceAnalytics(AAPL, fetch = false)).attempt.asserting { result =>
        result mustBe Left(AppError.PriceAnalyticsNotFound(AAPL))
      }
    }
  }
}
