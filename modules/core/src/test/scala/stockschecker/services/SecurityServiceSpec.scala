package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.Stream
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.common.config.SecurityConfig
import stockschecker.domain.{Exchange, JobId}
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.repositories.SecurityRepository
import stockschecker.repositories.SecurityRepository.ActivationResult
import stockschecker.fixtures.*

class SecurityServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "Security deactivation" should {
    "dispatch the same purge after a successful write is retried" in {
      val (repo, client, dispatcher) = mocks
      val id                         = JobId(s"purge-AAPL-$ts")
      val failure                    = new java.io.IOException("dispatch unavailable")
      when(repo.setActive(AAPL, false, ts, "provider-profile-empty")).thenReturnIO(ActivationResult.Applied)
      when(dispatcher.dispatchOnce(id, Action.PurgeStock(AAPL, ts))).thenReturn(IO.raiseError(failure), IO.unit)
      (for
        service <- SecurityService.make(repo, client, dispatcher)
        first   <- service.setActive(AAPL, false, ts, "provider-profile-empty").attempt
        _       <- service.setActive(AAPL, false, ts, "provider-profile-empty")
      yield first).asserting { first =>
        first mustBe Left(failure)
        verify(dispatcher, times(2)).dispatchOnce(id, Action.PurgeStock(AAPL, ts))
        succeed
      }
    }

    "refetch the profile after an accepted activation" in {
      val (repo, client, dispatcher) = mocks
      val action                     = Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL))
      val id                         = JobId(s"reactivate-AAPL-$ts")
      when(repo.setActive(AAPL, true, ts, "user-excluded")).thenReturnIO(ActivationResult.Applied)
      when(dispatcher.dispatchOnce(id, action)).thenReturnUnit
      SecurityService.make(repo, client, dispatcher).flatMap(_.setActive(AAPL, true, ts)).asserting { _ =>
        verify(dispatcher).dispatchOnce(id, action)
        succeed
      }
    }

    "reject deactivation of the shared benchmark before any write" in {
      val (repo, client, dispatcher) = mocks
      SecurityService.make(repo, client, dispatcher).flatMap(_.setActive(InvestmentEvaluator.Benchmark, false, ts)).attempt.asserting {
        result =>
          result.left.toOption.get mustBe a[AppError.FailedValidation]
          verifyNoInteractions(repo, client, dispatcher)
          succeed
      }
    }

    List(ActivationResult.Stale, ActivationResult.Delisted, ActivationResult.Missing).foreach { outcome =>
      s"avoid follow-up work for $outcome" in {
        val (repo, client, dispatcher) = mocks
        when(repo.setActive(AAPL, true, ts, "user-excluded")).thenReturnIO(outcome)
        SecurityService.make(repo, client, dispatcher).flatMap(_.setActive(AAPL, true, ts)).attempt.asserting { result =>
          result.isRight mustBe (outcome == ActivationResult.Stale)
          verifyNoInteractions(client, dispatcher)
          succeed
        }
      }
    }
  }

  "Listing reconciliation" should {
    "purge the active tickers delisted by a complete listing" in {
      val (repo, client, dispatcher) = mocks
      when(client.getTradedSecurities(Exchange.NASDAQ)).thenStream(MSFTSecurity)
      when(repo.countListed(Exchange.NASDAQ)).thenReturnIO(2L)
      when(repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))).thenReturnIO(List(AAPL))
      when(repo.pendingPurges).thenReturn(IO.pure(Nil), IO.pure(List(SecurityRepository.PendingPurge(AAPL, ts))))
      when(dispatcher.dispatchOnce(JobId(s"purge-AAPL-$ts"), Action.PurgeStock(AAPL, ts))).thenReturnUnit
      SecurityService
        .make(repo, client, dispatcher, SecurityConfig(0.5))
        .flatMap(_.fetchLatest(NonEmptyList.one(Exchange.NASDAQ)))
        .asserting { _ =>
          verify(dispatcher).dispatchOnce(JobId(s"purge-AAPL-$ts"), Action.PurgeStock(AAPL, ts))
          succeed
        }
    }

    List("dispatch", "acknowledgement").foreach { failurePoint =>
      s"replay the saved purge request after a failed $failurePoint without relying on newly delisted tickers" in {
        val (repo, client, dispatcher) = mocks
        val pending                    = List(SecurityRepository.PendingPurge(AAPL, ts))
        val id                         = JobId(s"purge-AAPL-$ts")
        val error                      = new java.io.IOException("temporary write failure")
        when(client.getTradedSecurities(Exchange.NASDAQ)).thenStream(MSFTSecurity)
        when(repo.countListed(Exchange.NASDAQ)).thenReturnIO(1L)
        when(repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))).thenReturn(IO.pure(List(AAPL)), IO.pure(Nil))
        when(repo.pendingPurges).thenReturn(IO.pure(Nil), IO.pure(pending), IO.pure(pending), IO.pure(Nil))
        when(dispatcher.dispatchOnce(id, Action.PurgeStock(AAPL, ts)))
          .thenReturn(if failurePoint == "dispatch" then IO.raiseError(error) else IO.unit, IO.unit)
        if failurePoint == "acknowledgement" then
          val _ = when(repo.acknowledgePurge(AAPL, ts)).thenReturn(IO.raiseError(error), IO.unit)
        (for
          service <- SecurityService.make(repo, client, dispatcher)
          first   <- service.fetchLatest(NonEmptyList.one(Exchange.NASDAQ)).attempt
          _       <- service.fetchLatest(NonEmptyList.one(Exchange.NASDAQ))
        yield first).asserting { first =>
          first mustBe Left(error)
          verify(dispatcher, times(2)).dispatchOnce(id, Action.PurgeStock(AAPL, ts))
          verify(repo, times(if failurePoint == "dispatch" then 1 else 2)).acknowledgePurge(AAPL, ts)
          succeed
        }
      }
    }

    "refuse a listing that shrinks beyond the configured fraction" in {
      val (repo, client, dispatcher) = mocks
      when(client.getTradedSecurities(Exchange.NASDAQ)).thenStream(MSFTSecurity)
      when(repo.countListed(Exchange.NASDAQ)).thenReturnIO(2L)
      SecurityService
        .make(repo, client, dispatcher, SecurityConfig(0.2))
        .flatMap(_.fetchLatest(NonEmptyList.one(Exchange.NASDAQ)))
        .attempt
        .asserting { result =>
          result.isLeft mustBe true
          verify(repo, never).reconcile(any[Exchange], anyList)
          verifyNoInteractions(dispatcher)
          succeed
        }
    }

    List("empty", "truncated").foreach { response =>
      s"leave state unchanged for an $response provider response" in {
        val (repo, client, dispatcher) = mocks
        val stream                     =
          if response == "empty" then Stream.empty
          else Stream.emit(AAPLSecurity) ++ Stream.raiseError[IO](new java.io.IOException("truncated"))
        when(client.getTradedSecurities(Exchange.NASDAQ)).thenReturn(stream)
        SecurityService.make(repo, client, dispatcher).flatMap(_.fetchLatest(NonEmptyList.one(Exchange.NASDAQ))).attempt.asserting {
          result =>
            result.isLeft mustBe true
            verify(repo, never).reconcile(any[Exchange], anyList)
            verifyNoInteractions(dispatcher)
            succeed
        }
      }
    }
  }

  private def mocks = {
    val repo = mock[SecurityRepository[IO]]
    when(repo.pendingPurges).thenReturnIO(Nil)
    when(repo.acknowledgePurge(any[stockschecker.domain.Ticker], any[java.time.Instant])).thenReturnUnit
    (repo, mock[MarketDataClient[IO]], mock[ActionDispatcher[IO]])
  }
}
