package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.domain.Stock
import stockschecker.repositories.StockRepository
import stockschecker.fixtures.*

class StockServiceSpec extends IOWordSpec {
  "A StockService" when {
    "seedStocks" should {
      "fetch traded stocks from client and store them in db" in {
        val (repo, client) = mocks
        when(client.getAllTradedStocks).thenReturnIO(List(AAPLStock))
        when(repo.save(anyList[Stock])).thenReturnUnit
        val res = for
          svc <- StockService.make(repo, client)
          _ <- svc.seedStocks
        yield ()

        res.asserting { r =>
          verify(client).getAllTradedStocks
          verify(repo).save(List(AAPLStock))
          r mustBe ()
        }
      }
    }
  }

  def mocks: (StockRepository[IO], MarketDataClient[IO]) =
    (mock[StockRepository[IO]], mock[MarketDataClient[IO]])
}
