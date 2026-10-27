package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.Clock
import stockschecker.actions.{Action, ActionDispatcher}
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.{InvestmentCandidate, StockFilters, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.StockRepository
import stockschecker.fixtures.*

class StockServiceSpec extends IOWordSpec {
  given Clock[IO] = Clock.mock(ts)

  "A StockService" when {
    "findInvestmentCandidates" should {
      "return joined investment candidates from its repository" in {
        val repo       = mock[StockRepository[IO]]
        val candidates = List(InvestmentCandidate(AAPLStock, None))
        when(repo.findInvestmentCandidates).thenReturnIO(candidates)
        StockService.make(repo, mock[ActionDispatcher[IO]]).flatMap(_.findInvestmentCandidates).asserting { result =>
          result mustBe candidates
          verify(repo).findInvestmentCandidates
          succeed
        }
      }
    }

    "findByTicker" should {
      "return stock when found" in {
        val repo = mock[StockRepository[IO]]
        when(repo.find(any[Ticker])).thenReturnSome(AAPLStock)

        val res = for
          svc <- StockService.make(repo, mock[ActionDispatcher[IO]])
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
          svc <- StockService.make(repo, mock[ActionDispatcher[IO]])
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
          svc <- StockService.make(repo, mock[ActionDispatcher[IO]])
          res <- svc.findAll(StockFilters(), Some(10))
        yield res

        res.asserting { stocks =>
          verify(repo).findAll(StockFilters(), Some(10))
          stocks mustBe List(AAPLStock, MSFTStock)
        }
      }
    }

    "activate" should {
      "dispatch activation after checking the stock exists" in {
        val repo       = mock[StockRepository[IO]]
        val dispatcher = mock[ActionDispatcher[IO]]
        when(repo.find(AAPL)).thenReturnSome(AAPLStock)
        when(dispatcher.dispatch(any[Action])).thenReturnUnit
        StockService.make(repo, dispatcher).flatMap(_.activate(AAPL)).asserting { _ =>
          verify(repo).find(AAPL)
          verify(dispatcher).dispatch(Action.SetSecurityActive(AAPL, true, ts))
          succeed
        }
      }

      "not dispatch activation for a missing stock" in {
        val repo       = mock[StockRepository[IO]]
        val dispatcher = mock[ActionDispatcher[IO]]
        when(repo.find(AAPL)).thenReturnNone
        StockService.make(repo, dispatcher).flatMap(_.activate(AAPL)).attempt.asserting { result =>
          result mustBe Left(AppError.SecurityNotFound(AAPL))
          verifyNoInteractions(dispatcher)
          succeed
        }
      }
    }

    "deactivate" should {
      "not dispatch deactivation for a missing stock" in {
        val repo       = mock[StockRepository[IO]]
        val dispatcher = mock[ActionDispatcher[IO]]
        when(repo.find(AAPL)).thenReturnNone
        StockService.make(repo, dispatcher).flatMap(_.deactivate(AAPL)).attempt.asserting { result =>
          result mustBe Left(AppError.SecurityNotFound(AAPL))
          verifyNoInteractions(dispatcher)
          succeed
        }
      }

      "dispatch deactivation after checking the stock exists" in {
        val repo       = mock[StockRepository[IO]]
        val dispatcher = mock[ActionDispatcher[IO]]
        when(repo.find(AAPL)).thenReturnSome(AAPLStock)
        when(dispatcher.dispatch(any[Action])).thenReturnUnit
        StockService.make(repo, dispatcher).flatMap(_.deactivate(AAPL)).asserting { _ =>
          verify(dispatcher).dispatch(Action.SetSecurityActive(AAPL, false, ts))
          verify(repo).find(AAPL)
          verifyNoMoreInteractions(dispatcher)
          succeed
        }
      }
    }
  }
}
