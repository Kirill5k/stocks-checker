package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{DateRange, PriceAdjustment, PriceAnalytics, PriceCandle, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{CompanyProfileRepository, LatestPriceRepository, PriceAnalyticsRepository, PriceHistoryRepository}
import stockschecker.fixtures.*
import java.time.{Instant, LocalDate, ZoneOffset}

class PriceServiceSpec extends IOWordSpec {
  private case class Dependencies(
      repo: PriceAnalyticsRepository[IO],
      latest: LatestPriceRepository[IO],
      client: MarketDataClient[IO],
      profiles: CompanyProfileRepository[IO],
      history: PriceHistoryRepository[IO]
  ) {
    def service: IO[PriceService[IO]] = PriceService.make(repo, latest, client, profiles, history)
    def cache(adjusted: PriceHistorySnapshot, quoted: PriceHistorySnapshot): Unit = {
      when(history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, adjusted.requestedThrough.minusYears(13)))
        .thenReturnIO(Some(adjusted))
      when(history.latest(AAPL, PriceAdjustment.Unadjusted)).thenReturnIO(Some(quoted))
      ()
    }
  }
  private def mocks: Dependencies = {
    val d = Dependencies(
      mock[PriceAnalyticsRepository[IO]],
      mock[LatestPriceRepository[IO]],
      mock[MarketDataClient[IO]],
      mock[CompanyProfileRepository[IO]],
      mock[PriceHistoryRepository[IO]]
    )
    when(d.repo.save(anyList[PriceAnalytics])).thenReturnUnit
    when(d.latest.save(anyList)).thenReturnUnit
    when(d.profiles.updatePriceAnalyticsLastUpdated(anyList)).thenReturnUnit
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
      "TwelveData",
      "USD",
      adjustment,
      now,
      through.minusYears(13),
      through
    )
  }

  "PriceService" should {
    "reuse daily observations and save the unadjusted quote with direct profile updates" in {
      val d = mocks
      (for
        now <- IO.realTimeInstant
        _ = d.cache(snapshot(now, PriceAdjustment.SplitsAndDividends), snapshot(now, PriceAdjustment.Unadjusted))
        service   <- d.service
        analytics <- service.findPriceAnalytics(AAPL, fetch = true)
      yield analytics).asserting { analytics =>
        analytics.performanceSummary.latestPrice mustBe BigDecimal(100)
        analytics.basis.flatMap(_.priceObservationId) mustBe Some("split-and-dividend")
        analytics.basis.flatMap(_.quotedPriceObservationId) mustBe Some("none")
        verify(d.repo).save(List(analytics))
        verify(d.latest).save(List(analytics.toLatestPrice))
        verify(d.profiles).updatePriceAnalyticsLastUpdated(List(AAPL))
        verifyNoInteractions(d.client)
        succeed
      }
    }

    "align a cached adjusted series and a later quote to their latest common trading session" in {
      val d = mocks
      (for
        now <- IO.realTimeInstant
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
        now <- IO.realTimeInstant
        _ = d.cache(snapshot(now, PriceAdjustment.SplitsAndDividends, List(0L)), snapshot(now, PriceAdjustment.Unadjusted, List(1L)))
        service <- d.service
        outcome <- service.findPriceAnalytics(AAPL, fetch = true).attempt
      yield outcome).asserting { outcome =>
        outcome mustBe Left(AppError.DataUnavailable("TwelveData", AAPL, "adjusted and quoted histories have no common trading session"))
        verify(d.repo, never).save(anyList[PriceAnalytics])
        verify(d.latest, never).save(anyList)
        verify(d.profiles, never).updatePriceAnalyticsLastUpdated(anyList)
        verifyNoInteractions(d.client)
        succeed
      }
    }

    "report a halted ticker's stale session as unavailable data without indefinitely deferring its job" in {
      val d = mocks
      (for
        now <- IO.realTimeInstant
        _ = d.cache(snapshot(now, PriceAdjustment.SplitsAndDividends, List(45L)), snapshot(now, PriceAdjustment.Unadjusted, List(45L)))
        service <- d.service
        outcome <- service.findPriceAnalytics(AAPL, fetch = true).attempt
      yield outcome).asserting { outcome =>
        outcome mustBe Left(AppError.DataUnavailable("TwelveData", AAPL, "the latest matching trading session is more than seven days old"))
        verify(d.repo, never).save(anyList[PriceAnalytics])
        verify(d.latest, never).save(anyList)
        verify(d.profiles, never).updatePriceAnalyticsLastUpdated(anyList)
        verifyNoInteractions(d.client)
        succeed
      }
    }

    "reject mismatched currencies without publishing analytics" in {
      val d = mocks
      (for
        now <- IO.realTimeInstant
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
        now <- IO.realTimeInstant
        full = snapshot(now, PriceAdjustment.SplitsAndDividends)
        _    = d.cache(full.copy(requestedFrom = full.requestedThrough.minusDays(14)), snapshot(now, PriceAdjustment.Unadjusted))
        _    = when(
          d.client.getDailyPriceHistory(AAPL, DateRange(full.requestedFrom, full.requestedThrough), PriceAdjustment.SplitsAndDividends)
        ).thenReturnIO(full)
        service <- d.service
        _       <- service.findPriceAnalytics(AAPL, fetch = true)
      yield full).asserting { full =>
        verify(d.history).save(full)
        verify(d.client).getDailyPriceHistory(
          AAPL,
          DateRange(full.requestedFrom, full.requestedThrough),
          PriceAdjustment.SplitsAndDividends
        )
        succeed
      }
    }

    "retain the first fetched observation when the second request is rate limited" in {
      val d = mocks
      (for
        now <- IO.realTimeInstant
        full = snapshot(now, PriceAdjustment.SplitsAndDividends)
        _    = when(d.history.latest(any[Ticker], any[PriceAdjustment])).thenReturnIO(None)
        _    = when(d.history.latestCovering(AAPL, PriceAdjustment.SplitsAndDividends, full.requestedFrom)).thenReturnIO(None)
        _    = when(
          d.client.getDailyPriceHistory(AAPL, DateRange(full.requestedFrom, full.requestedThrough), PriceAdjustment.SplitsAndDividends)
        ).thenReturnIO(full)
        deferred = AppError.ProviderDeferred("TwelveData", now.plusSeconds(60), "quota")
        _        = when(
          d.client
            .getDailyPriceHistory(AAPL, DateRange(full.requestedThrough.minusDays(14), full.requestedThrough), PriceAdjustment.Unadjusted)
        )
          .thenRaiseError(deferred)
        service <- d.service
        result  <- service.findPriceAnalytics(AAPL, fetch = true).attempt
      yield (full, deferred, result)).asserting { case (full, deferred, result) =>
        result mustBe Left(deferred)
        verify(d.history).save(full)
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
