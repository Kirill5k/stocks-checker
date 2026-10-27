package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.Stream
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{Exchange, ListingStatus, Security}
import stockschecker.repositories.SecurityRepository
import stockschecker.fixtures.*

class SecurityServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A SecurityService" when {
    "fetchLatestSecurities" should {
      "fetch traded securities from client and store them in db" in {
        val (repo, client, dispatcher) = mocks
        when(client.getTradedSecurities(any[Exchange])).thenStream(AAPLSecurity)
        when(repo.reconcile(any[Exchange], anyList[Security])).thenReturnUnit
        val res = for
          svc <- SecurityService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(Exchange.NASDAQ))
        yield ()

        res.asserting { r =>
          verify(client).getTradedSecurities(Exchange.NASDAQ)
          verify(repo).reconcile(Exchange.NASDAQ, List(AAPLSecurity))
          r mustBe ()
        }
      }

      "leave listing state unchanged when a provider stream fails partway through" in {
        val (repo, client, dispatcher) = mocks
        val error                      = new RuntimeException("Truncated listing response")
        when(client.getTradedSecurities(Exchange.NASDAQ)).thenReturn(
          Stream.emit(AAPLSecurity).covary[IO] ++ Stream.raiseError[IO](error)
        )
        val result = for
          svc <- SecurityService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(Exchange.NASDAQ))
        yield ()
        result.attempt.asserting { outcome =>
          outcome mustBe Left(error)
          verifyNoInteractions(repo)
          succeed
        }
      }

      "refuse to reconcile an unexpectedly empty exchange listing" in {
        val (repo, client, dispatcher) = mocks
        when(client.getTradedSecurities(Exchange.NASDAQ)).thenReturn(Stream.empty)
        val result = for
          svc <- SecurityService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(NonEmptyList.of(Exchange.NASDAQ))
        yield ()
        result.attempt.asserting { outcome =>
          outcome.isLeft mustBe true
          verifyNoInteractions(repo)
          succeed
        }
      }
    }

    "recordCompanyProfileUpdate" should {
      "update the owning repository for every ticker in the event" in {
        val (repo, client, dispatcher) = mocks
        val tickers                    = NonEmptyList.of(AAPL, MSFT)
        when(repo.updateCompanyProfileLastUpdated(tickers.toList)).thenReturnUnit
        (for {
          service <- SecurityService.make(repo, client, dispatcher)
          _       <- service.recordCompanyProfileUpdate(tickers)
        } yield ()).asserting { _ =>
          verify(repo).updateCompanyProfileLastUpdated(tickers.toList)
          verifyNoInteractions(client, dispatcher)
          succeed
        }
      }
    }

    "setActive" should {
      "deactivate the security and pass the original request time to its profile" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(AAPL)).thenReturnSome(AAPLSecurity)
        when(repo.setActive(AAPL, false, ts)).thenReturnUnit
        when(dispatcher.dispatch(Action.SetCompanyProfileActive(AAPL, false, ts))).thenReturnUnit
        (for
          service <- SecurityService.make(repo, client, dispatcher)
          _       <- service.setActive(AAPL, false, ts)
        yield ()).asserting { _ =>
          verify(repo).setActive(AAPL, false, ts)
          verify(dispatcher).dispatch(Action.SetCompanyProfileActive(AAPL, false, ts))
          verifyNoInteractions(client)
          succeed
        }
      }

      "activate a listed security and dispatch profile activation" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(AAPL)).thenReturnIO(Some(AAPLSecurity))
        when(repo.setActive(AAPL, true, ts)).thenReturnUnit
        when(dispatcher.dispatch(Action.SetCompanyProfileActive(AAPL, true, ts))).thenReturnUnit
        (for {
          service <- SecurityService.make(repo, client, dispatcher)
          _       <- service.setActive(AAPL, true, ts)
        } yield ()).asserting { _ =>
          verify(repo).find(AAPL)
          verify(repo).setActive(AAPL, true, ts)
          verify(dispatcher).dispatch(Action.SetCompanyProfileActive(AAPL, true, ts))
          verifyNoInteractions(client)
          succeed
        }
      }

      "send an inactive profile state when activation is requested for a delisted security" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(AAPL)).thenReturnIO(Some(AAPLSecurity.copy(listingStatus = ListingStatus.Delisted)))
        when(repo.setActive(AAPL, true, ts)).thenReturnUnit
        when(dispatcher.dispatch(Action.SetCompanyProfileActive(AAPL, false, ts))).thenReturnUnit
        (for {
          service <- SecurityService.make(repo, client, dispatcher)
          _       <- service.setActive(AAPL, true, ts)
        } yield ()).asserting { _ =>
          verify(repo).find(AAPL)
          verify(repo).setActive(AAPL, true, ts)
          verify(dispatcher).dispatch(Action.SetCompanyProfileActive(AAPL, false, ts))
          verifyNoInteractions(client)
          succeed
        }
      }

      "reject a missing security before changing state" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(AAPL)).thenReturnIO(None)
        (for {
          service <- SecurityService.make(repo, client, dispatcher)
          result  <- service.setActive(AAPL, true, ts).attempt
        } yield result).asserting { result =>
          result mustBe Left(stockschecker.domain.errors.AppError.SecurityNotFound(AAPL))
          verify(repo).find(AAPL)
          verifyNoMoreInteractions(repo)
          verifyNoInteractions(client, dispatcher)
          succeed
        }
      }
    }
  }

  def mocks: (SecurityRepository[IO], MarketDataClient[IO], ActionDispatcher[IO]) =
    (mock[SecurityRepository[IO]], mock[MarketDataClient[IO]], mock[ActionDispatcher[IO]])
}
