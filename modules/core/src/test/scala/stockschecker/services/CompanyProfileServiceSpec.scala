package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.Ticker
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
    }
  }

  def mocks: (CompanyProfileRepository[IO], MarketDataClient[IO]) =
    (mock[CompanyProfileRepository[IO]], mock[MarketDataClient[IO]])
}
