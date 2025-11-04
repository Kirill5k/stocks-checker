package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.repositories.CompanyProfileRepository
import stockschecker.fixtures.*

class CompanyProfileServiceSpec extends IOWordSpec {
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
          cp mustBe AAPLCompanyProfile
        }
      }

      "fetch latest company profile when flag is true" in {
        val (repo, client, dispatcher) = mocks
        when(client.getCompanyProfile(any[Ticker])).thenReturnSome(AAPLCompanyProfile)
        when(repo.save(anyList[CompanyProfile])).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.get(AAPL, true)
        yield res

        res.asserting { cp =>
          verify(client).getCompanyProfile(AAPL)
          verify(repo).save(List(AAPLCompanyProfile))
          verify(dispatcher).dispatch(Action.RecordCompanyProfileUpdate(List(AAPL)))
          verifyNoMoreInteractions(repo) // ensure repo.find wasn't called
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

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.get(AAPL, true) // fetch latest path
        yield res

        res.attempt.asserting { err =>
          verify(client).getCompanyProfile(AAPL)
          // repo.save should not be called, and repo.find not used
          verifyNoInteractions(repo)
          verifyNoInteractions(dispatcher)
          err mustBe Left(AppError.CompanyProfileNotFound(AAPL))
        }
      }
    }

    "getAll" should {
      "return all company profiles from repository" in {
        val (repo, client, dispatcher) = mocks
        when(repo.findAll(any[Option[Int]])).thenReturnIO(List(AAPLCompanyProfile, MSFTCompanyProfile))

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.getAll(None)
        yield res

        res.asserting { profiles =>
          verify(repo).findAll(None)
          profiles mustBe List(AAPLCompanyProfile, MSFTCompanyProfile)
        }
      }

      "return limited number of company profiles when limit is specified" in {
        val (repo, client, dispatcher) = mocks
        when(repo.findAll(any[Option[Int]])).thenReturnIO(List(AAPLCompanyProfile))

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.getAll(Some(1))
        yield res

        res.asserting { profiles =>
          verify(repo).findAll(Some(1))
          profiles mustBe List(AAPLCompanyProfile)
        }
      }

      "return empty list when no company profiles exist" in {
        val (repo, client, dispatcher) = mocks
        when(repo.findAll(any[Option[Int]])).thenReturnIO(List.empty)

        val res = for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          res <- svc.getAll(None)
        yield res

        res.asserting { profiles =>
          verify(repo).findAll(None)
          profiles mustBe List.empty
        }
      }
    }

    "fetchLatest" should {
      "fetch company profiles and dispatch RecordCompanyProfileUpdate action" in {
        val (repo, client, dispatcher) = mocks
        val tickers                    = NonEmptyList.of(AAPL, MSFT)
        when(client.getCompanyProfile(any[Ticker]))
          .thenReturnSome(AAPLCompanyProfile)
          .thenReturnSome(MSFTCompanyProfile)
        when(repo.save(anyList[CompanyProfile])).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        (for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(tickers)
        yield ()).asserting { _ =>
          verify(client).getCompanyProfile(AAPL)
          verify(client).getCompanyProfile(MSFT)
          verify(repo).save(List(AAPLCompanyProfile, MSFTCompanyProfile))
          verify(dispatcher).dispatch(Action.RecordCompanyProfileUpdate(List(AAPL, MSFT)))
          succeed
        }
      }

      "handle errors gracefully and still dispatch action for successful fetches" in {
        val (repo, client, dispatcher) = mocks
        val tickers                    = NonEmptyList.of(AAPL, MSFT)
        when(client.getCompanyProfile(AAPL)).thenReturnSome(AAPLCompanyProfile)
        when(client.getCompanyProfile(MSFT)).thenReturn(IO.raiseError(new RuntimeException("API error")))
        when(repo.save(anyList[CompanyProfile])).thenReturnUnit
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        (for
          svc <- CompanyProfileService.make(repo, client, dispatcher)
          _   <- svc.fetchLatest(tickers)
        yield ()).asserting { _ =>
          verify(client).getCompanyProfile(AAPL)
          verify(client).getCompanyProfile(MSFT)
          verify(repo).save(List(AAPLCompanyProfile))
          verify(dispatcher).dispatch(Action.RecordCompanyProfileUpdate(List(AAPL)))
          succeed
        }
      }
    }
  }

  def mocks: (CompanyProfileRepository[IO], MarketDataClient[IO], ActionDispatcher[IO]) =
    (mock[CompanyProfileRepository[IO]], mock[MarketDataClient[IO]], mock[ActionDispatcher[IO]])
}
