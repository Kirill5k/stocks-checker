package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{StockFilters, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.StockRepository
import stockschecker.fixtures.*

class StockServiceSpec extends IOWordSpec {

  "A StockService" when {
    "findByTicker" should {
      "return stock when found" in {
        val (repo, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnSome(AAPLStock)

        val res = for
          svc <- StockService.make(repo, dispatcher)
          res <- svc.findByTicker(AAPL)
        yield res

        res.asserting { stock =>
          verify(repo).find(AAPL)
          stock mustBe AAPLStock
        }
      }

      "raise error when not found" in {
        val (repo, dispatcher) = mocks
        when(repo.find(any[Ticker])).thenReturnNone

        val res = for
          svc <- StockService.make(repo, dispatcher)
          res <- svc.findByTicker(AAPL)
        yield res

        res.assertThrows(AppError.SecurityNotFound(AAPL))
      }
    }

    "findAll" should {
      "return all stocks matching filters" in {
        val (repo, dispatcher) = mocks
        when(repo.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock, MSFTStock))

        val res = for
          svc <- StockService.make(repo, dispatcher)
          res <- svc.findAll(StockFilters(), Some(10))
        yield res

        res.asserting { stocks =>
          verify(repo).findAll(StockFilters(), Some(10))
          stocks mustBe List(AAPLStock, MSFTStock)
        }
      }
    }

    "deactivate" should {
      "dispatch DeactivateSecurity and DeactivateCompanyProfile actions" in {
        val (repo, dispatcher) = mocks
        when(dispatcher.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- StockService.make(repo, dispatcher)
          _   <- svc.deactivate(AAPL)
        yield ()

        res.asserting { _ =>
          verify(dispatcher).dispatch(Action.DeactivateSecurity(AAPL))
          verify(dispatcher).dispatch(Action.DeactivateCompanyProfile(AAPL))
          succeed
        }
      }
    }
  }

  def mocks: (StockRepository[IO], ActionDispatcher[IO]) =
    (mock[StockRepository[IO]], mock[ActionDispatcher[IO]])
}
