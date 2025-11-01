package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.repositories.CompanyProfileRepository
import stockschecker.fixtures.*

class CompanyProfileServiceSpec extends IOWordSpec {

  "A CompanyProfileService" when {
    "get" should {
      "get company profile from database" in {
        val (repo, client) = mocks
        when(repo.find(any[Ticker])).thenReturnSome(AAPLCompanyProfile)

        val res = for
          svc <- CompanyProfileService.make(repo, client)
          res <- svc.get(AAPL)
        yield res

        res.asserting { cp =>
          verify(repo).find(AAPL)
          cp mustBe AAPLCompanyProfile
        }
      }

      "fetch latest company profile" in {
        val (repo, client) = mocks
        when(client.getCompanyProfile(any[Ticker])).thenReturnSome(AAPLCompanyProfile)
        when(repo.save(any[CompanyProfile])).thenReturnUnit

        val res = for
          svc <- CompanyProfileService.make(repo, client)
          res <- svc.get(AAPL, true)
        yield res

        res.asserting { cp =>
          verify(client).getCompanyProfile(AAPL)
          verify(repo).save(AAPLCompanyProfile)
          cp mustBe AAPLCompanyProfile
        }
      }

      "fetch company profile from client if it is not present in db" in {
        val (repo, client) = mocks
        when(repo.find(any[Ticker])).thenReturnNone
        when(repo.save(any[CompanyProfile])).thenReturnUnit
        when(client.getCompanyProfile(any[Ticker])).thenReturnSome(AAPLCompanyProfile)

        val res = for
          svc <- CompanyProfileService.make(repo, client)
          res <- svc.get(AAPL)
        yield res

        res.asserting { cp =>
          verify(repo).find(AAPL)
          verify(client).getCompanyProfile(AAPL)
          verify(repo).save(AAPLCompanyProfile)
          cp mustBe AAPLCompanyProfile
        }
      }

      "return errors if company profile is missing" in {
        val (repo, client) = mocks
        when(repo.find(any[Ticker])).thenReturnNone
        when(client.getCompanyProfile(any[Ticker])).thenReturnNone

        val res = for
          svc <- CompanyProfileService.make(repo, client)
          res <- svc.get(AAPL)
        yield res

        res.attempt.asserting { err =>
          verify(repo).find(AAPL)
          verify(client).getCompanyProfile(AAPL)
          verifyNoMoreInteractions(repo)
          err mustBe Left(AppError.CompanyProfileNotFound(AAPL))
        }
      }
    }

    "getAll" should {
      "return all company profiles from repository" in {
        val (repo, client) = mocks
        when(repo.findAll(any[Option[Int]])).thenReturnIO(List(AAPLCompanyProfile, MSFTCompanyProfile))

        val res = for
          svc <- CompanyProfileService.make(repo, client)
          res <- svc.getAll(None)
        yield res

        res.asserting { profiles =>
          verify(repo).findAll(None)
          profiles mustBe List(AAPLCompanyProfile, MSFTCompanyProfile)
        }
      }

      "return limited number of company profiles when limit is specified" in {
        val (repo, client) = mocks
        when(repo.findAll(any[Option[Int]])).thenReturnIO(List(AAPLCompanyProfile))

        val res = for
          svc <- CompanyProfileService.make(repo, client)
          res <- svc.getAll(Some(1))
        yield res

        res.asserting { profiles =>
          verify(repo).findAll(Some(1))
          profiles mustBe List(AAPLCompanyProfile)
        }
      }

      "return empty list when no company profiles exist" in {
        val (repo, client) = mocks
        when(repo.findAll(any[Option[Int]])).thenReturnIO(List.empty)

        val res = for
          svc <- CompanyProfileService.make(repo, client)
          res <- svc.getAll(None)
        yield res

        res.asserting { profiles =>
          verify(repo).findAll(None)
          profiles mustBe List.empty
        }
      }
    }
  }

  def mocks: (CompanyProfileRepository[IO], MarketDataClient[IO]) =
    (mock[CompanyProfileRepository[IO]], mock[MarketDataClient[IO]])
}
