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
  }

  def mocks: (CompanyProfileRepository[IO], MarketDataClient[IO]) =
    (mock[CompanyProfileRepository[IO]], mock[MarketDataClient[IO]])
}
