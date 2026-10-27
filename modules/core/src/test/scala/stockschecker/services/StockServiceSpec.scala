package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.{StockFilters, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{CompanyProfileRepository, SecurityRepository, StockRepository}
import stockschecker.fixtures.*

class StockServiceSpec extends IOWordSpec {

  "A StockService" when {
    "findByTicker" should {
      "return stock when found" in {
        val repo = mock[StockRepository[IO]]
        when(repo.find(any[Ticker])).thenReturnSome(AAPLStock)

        val res = for
          svc <- StockService.make(repo, mock[SecurityRepository[IO]], mock[CompanyProfileRepository[IO]])
          res <- svc.findByTicker(AAPL)
        yield res

        res.asserting { stock =>
          verify(repo).find(AAPL)
          stock mustBe AAPLStock
        }
      }

      "raise error when not found" in {
        val repo = mock[StockRepository[IO]]
        when(repo.find(any[Ticker])).thenReturnNone

        val res = for
          svc <- StockService.make(repo, mock[SecurityRepository[IO]], mock[CompanyProfileRepository[IO]])
          res <- svc.findByTicker(AAPL)
        yield res

        res.assertThrows(AppError.SecurityNotFound(AAPL))
      }
    }

    "findAll" should {
      "return all stocks matching filters" in {
        val repo = mock[StockRepository[IO]]
        when(repo.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock, MSFTStock))

        val res = for
          svc <- StockService.make(repo, mock[SecurityRepository[IO]], mock[CompanyProfileRepository[IO]])
          res <- svc.findAll(StockFilters(), Some(10))
        yield res

        res.asserting { stocks =>
          verify(repo).findAll(StockFilters(), Some(10))
          stocks mustBe List(AAPLStock, MSFTStock)
        }
      }
    }

    "deactivate" should {
      "persist both deactivations before returning" in {
        val repo       = mock[StockRepository[IO]]
        val securities = mock[SecurityRepository[IO]]
        val profiles   = mock[CompanyProfileRepository[IO]]
        when(securities.deactivate(AAPL)).thenReturnUnit
        when(profiles.deactivate(AAPL)).thenReturnUnit
        StockService.make(repo, securities, profiles).flatMap(_.deactivate(AAPL)).asserting { _ =>
          verify(securities).deactivate(AAPL)
          verify(profiles).deactivate(AAPL)
          verifyNoInteractions(repo)
          succeed
        }
      }
    }
  }
}
