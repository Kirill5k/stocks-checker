package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.actions.{Action, ActionDispatcher}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.repositories.CompanyProfileRepository
import stockschecker.fixtures.*

class CompanyProfileServiceSpec extends IOWordSpec {
  given Clock[IO]  = Clock.mock(ts)
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A CompanyProfileService" when {
    "get" should {
      "get company profile from database" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnSome(AAPLCompanyProfile)

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.get(AAPL)
        yield res

        res.asserting { cp =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client, dispatcher)
          cp mustBe AAPLCompanyProfile
        }
      }

      "fetch and return the client profile without reading the repository when flag is true" in {
        val (repo, client, dispatcher) = mocks
        when(client.getCompanyProfile(any[Ticker])).thenReturnSome(AAPLCompanyProfile)
        when(repo.save(AAPLCompanyProfile)).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.get(AAPL, true)
        yield res

        res.asserting { cp =>
          verify(client).getCompanyProfile(AAPL)
          verify(repo).save(AAPLCompanyProfile)
          verify(dispatcher).dispatch(Action.RecordCompanyProfileUpdate(NonEmptyList.one(AAPL)))
          verify(repo, never).find(AAPL)
          cp mustBe AAPLCompanyProfile
        }
      }

      "return error when company profile missing in db and fetchLatest flag is false" in {
        val (repo, client, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnNone
        // client should NOT be called

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.get(AAPL) // flag is false (default)
        yield res

        res.attempt.asserting { err =>
          verify(repo).find(AAPL)
          verifyNoInteractions(client)
          verifyNoInteractions(dispatcher)
          err mustBe Left(AppError.CompanyProfileNotFound(AAPL))
        }
      }

      "return error when explicitly fetching latest and client returns nothing" in {
        val (repo, client, dispatcher) = mocks
        when(client.getCompanyProfile(any[Ticker])).thenReturnNone
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.get(AAPL, true) // fetch latest path
        yield res

        res.attempt.asserting { err =>
          verify(client).getCompanyProfile(AAPL)
          verify(repo, never).save(any[CompanyProfile])
          verify(repo, never).find(AAPL)
          verify(dispatcher).dispatch(Action.SetSecurityActive(AAPL, false, ts, "provider-profile-empty"))
          verify(dispatcher, never).dispatch(Action.RecordCompanyProfileUpdate(NonEmptyList.one(AAPL)))
          err mustBe Left(AppError.CompanyProfileNotFound(AAPL))
        }
      }
    }

    "fetch latest" should {
      "bootstrap a profile without requiring a cached profile" in {
        val (repo, client, dispatcher) = mocks
        when(client.getCompanyProfile(AAPL)).thenReturnSome(AAPLCompanyProfile)
        when(repo.save(AAPLCompanyProfile)).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit
        CompanyProfileService.make(repo, client, dispatcher).flatMap(_.get(AAPL, true)).asserting { result =>
          result mustBe AAPLCompanyProfile
          verify(repo).save(AAPLCompanyProfile)
          verify(repo, never).find(AAPL)
          verify(dispatcher).dispatch(Action.RecordCompanyProfileUpdate(NonEmptyList.one(AAPL)))
          verify(dispatcher, never).dispatch(Action.SetSecurityActive(AAPL, true, ts))
          succeed
        }
      }

      "leave activation unchanged for provider failures" in {
        val (repo, client, dispatcher) = mocks
        val error                      = new java.io.IOException("Provider unavailable")
        when(client.getCompanyProfile(AAPL)).thenRaiseError(error)
        CompanyProfileService.make(repo, client, dispatcher).flatMap(_.get(AAPL, true)).attempt.asserting { result =>
          result mustBe Left(error)
          verifyNoInteractions(repo, dispatcher)
          succeed
        }
      }

    }

  }

  def mocks: (CompanyProfileRepository[IO], MarketDataClient[IO], ActionDispatcher[IO]) =
    (mock[CompanyProfileRepository[IO]], mock[MarketDataClient[IO]], mock[ActionDispatcher[IO]])
}
