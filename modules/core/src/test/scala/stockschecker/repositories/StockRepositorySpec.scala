package stockschecker.repositories

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.embedded.EmbeddedMongo
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.fixtures.*

import scala.concurrent.Future

class StockRepositorySpec extends AsyncWordSpec with Matchers with EmbeddedMongo {

  "A StockRepository" when {
    "save" should {
      "store single stock in database" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- StockRepository.make(db)
            _    <- repo.save(AAPLStock)
            all  <- repo.streamAll.compile.toList
          yield all mustBe List(AAPLStock)
        }
      }

      "store multiple stocks in database" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- StockRepository.make(db)
            stocks = List(AAPLStock, MSFTStock)
            _   <- repo.save(stocks)
            all <- repo.streamAll.compile.toList
          yield all mustBe stocks
        }
      }
    }

    "find" should {
      "return empty option when stock is not found" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- StockRepository.make(db)
            s    <- repo.find(AAPL)
          yield s mustBe None
        }
      }

      "find stock by ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- StockRepository.make(db)
            _    <- repo.save(AAPLStock)
            s    <- repo.find(AAPL)
          yield s mustBe Some(AAPLStock)
        }
      }
    }
  }

  def withEmbeddedMongoDatabase[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo(12146) {
      MongoClient
        .fromConnectionString[IO]("mongodb://localhost:12146")
        .evalMap(_.getDatabase("stock-checker"))
        .use(test)
    }.unsafeToFuture()(IORuntime.global)
}
