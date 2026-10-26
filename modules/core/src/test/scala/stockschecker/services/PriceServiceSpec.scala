package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PriceAnalytics, PriceCandle, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{LatestPriceRepository, PriceAnalyticsRepository, PriceHistoryRepository}
import stockschecker.fixtures.*
import java.time.{LocalDate, ZoneOffset}

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

      "report errors when fetching price candles fails" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val error                                       = new RuntimeException("API error")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          _   <- svc.fetchLatestPriceAnalytics(NonEmptyList.of(AAPL))
        yield ()

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verifyNoInteractions(repo, latestPriceRepo)
          result mustBe Left(error)
        }
      }

      "report unavailable provider prices without deactivating the security" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val error = stockschecker.domain.errors.AppError.HttpClient("TwelveData", 404, "Ticker not found")
        when(client.getMonthlyPriceCandles(any[Ticker])).thenRaiseError(error)

        val res = for
          svc <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          _   <- svc.fetchLatestPriceAnalytics(NonEmptyList.of(AAPL))
        yield ()

        res.attempt.asserting { result =>
          verify(client).getMonthlyPriceCandles(AAPL)
          verifyNoInteractions(repo, latestPriceRepo, dispatcher)
          result mustBe Left(error)
        }
      }

      "keep the successful price update when another ticker fails" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val error                                       = new RuntimeException("Price provider unavailable")
        when(client.getMonthlyPriceCandles(AAPL)).thenReturnIO(AAPLPriceCandles)
        when(client.getMonthlyPriceCandles(MSFT)).thenRaiseError(error)
        when(repo.save(anyList[PriceAnalytics])).thenReturnUnit
        when(latestPriceRepo.save(anyList)).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit
        val result = for
          svc <- PriceService.make(repo, latestPriceRepo, client, dispatcher)
          _   <- svc.fetchLatestPriceAnalytics(NonEmptyList.of(AAPL, MSFT))
        yield ()
        result.attempt.asserting { outcome =>
          outcome mustBe Left(error)
          verify(repo).save(List(AAPLPriceAnalytics))
          verify(dispatcher).dispatch(Action.RecordPriceAnalyticsUpdate(List(AAPL)))
          succeed
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
      "reuse cached daily observations and retain a distinct unadjusted quote" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val history                                     = mock[PriceHistoryRepository[IO]]
        when(repo.save(anyList[PriceAnalytics])).thenReturnUnit
        when(latestPriceRepo.save(anyList)).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit
        val result = for
          now <- IO.realTimeInstant
          through  = LocalDate.ofInstant(now, ZoneOffset.UTC)
          adjusted = PriceHistorySnapshot(
            "adjusted",
            AAPL,
            List(PriceCandle(through, 50, 50, 50, 50, 1L)),
            "TwelveData",
            "USD",
            "split-and-dividend",
            now,
            through.minusYears(13),
            through
          )
          quoted = adjusted.copy(
            id = "quote",
            adjustment = "none",
            requestedFrom = through.minusDays(14),
            candles = List(PriceCandle(through, 100, 100, 100, 100, 1L))
          )
          _ = when(history.latest(AAPL, "split-and-dividend")).thenReturnIO(Some(adjusted))
          _ = when(history.latest(AAPL, "none")).thenReturnIO(Some(quoted))
          svc       <- PriceService.make(repo, latestPriceRepo, client, dispatcher, Some(history))
          analytics <- svc.findPriceAnalytics(AAPL, fetch = true)
        yield analytics
        result.asserting { analytics =>
          analytics.performanceSummary.latestPrice mustBe BigDecimal(100)
          analytics.basis.flatMap(_.priceObservationId) mustBe Some("adjusted")
          analytics.basis.flatMap(_.quotedPriceObservationId) mustBe Some("quote")
          verifyNoInteractions(client)
          succeed
        }
      }

      "refetch a fresh cached history when it does not cover the requested horizon" in {
        val (repo, latestPriceRepo, client, dispatcher) = mocks
        val history                                     = mock[PriceHistoryRepository[IO]]
        when(repo.save(anyList[PriceAnalytics])).thenReturnUnit
        when(latestPriceRepo.save(anyList)).thenReturnUnit
        when(dispatcher.dispatch(any)).thenReturnUnit
        when(history.save(any[PriceHistorySnapshot])).thenReturnUnit
        val result = for
          now <- IO.realTimeInstant
          through = LocalDate.ofInstant(now, ZoneOffset.UTC)
          full    = PriceHistorySnapshot(
            "full",
            AAPL,
            List(PriceCandle(through, 100, 100, 100, 100, 1L)),
            "TwelveData",
            "USD",
            "split-and-dividend",
            now,
            through.minusYears(13),
            through
          )
          short  = full.copy(id = "short", requestedFrom = through.minusDays(14))
          quoted = short.copy(id = "quote", adjustment = "none")
          _      = when(history.latest(AAPL, "split-and-dividend")).thenReturnIO(Some(short))
          _      = when(history.latest(AAPL, "none")).thenReturnIO(Some(quoted))
          _      = when(client.getDailyPriceHistory(AAPL, through.minusYears(13), through, "all")).thenReturnIO(full)
          svc       <- PriceService.make(repo, latestPriceRepo, client, dispatcher, Some(history))
          analytics <- svc.findPriceAnalytics(AAPL, fetch = true)
        yield (analytics, full)
        result.asserting { case (analytics, full) =>
          analytics.basis.flatMap(_.priceObservationId) mustBe Some("full")
          verify(history).save(full)
          verify(client).getDailyPriceHistory(AAPL, full.requestedFrom, full.requestedThrough, "all")
          succeed
        }
      }

      "reject adjusted and quoted histories with inconsistent dates or currencies" in {
        def run(currency: String, dateOffset: Long): IO[Either[Throwable, PriceAnalytics]] = {
          val (repo, latestPriceRepo, client, dispatcher) = mocks
          val history                                     = mock[PriceHistoryRepository[IO]]
          for
            now <- IO.realTimeInstant
            through  = LocalDate.ofInstant(now, ZoneOffset.UTC)
            adjusted = PriceHistorySnapshot(
              "adjusted",
              AAPL,
              List(PriceCandle(through, 100, 100, 100, 100, 1L)),
              "TwelveData",
              "USD",
              "split-and-dividend",
              now,
              through.minusYears(13),
              through
            )
            quoted = adjusted.copy(
              id = "quote",
              currency = currency,
              adjustment = "none",
              requestedFrom = through.minusDays(14),
              candles = List(PriceCandle(through.minusDays(dateOffset), 100, 100, 100, 100, 1L))
            )
            _ = when(history.latest(AAPL, "split-and-dividend")).thenReturnIO(Some(adjusted))
            _ = when(history.latest(AAPL, "none")).thenReturnIO(Some(quoted))
            svc     <- PriceService.make(repo, latestPriceRepo, client, dispatcher, Some(history))
            outcome <- svc.findPriceAnalytics(AAPL, fetch = true).attempt
            _ = verifyNoInteractions(repo, latestPriceRepo, dispatcher, client)
          yield outcome
        }
        (for
          currencyMismatch <- run("CAD", 0)
          dateMismatch     <- run("USD", 1)
        yield List(currencyMismatch, dateMismatch)).asserting { outcomes =>
          outcomes.foreach(_.left.toOption.get mustBe a[AppError.FailedValidation])
          succeed
        }
      }

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
