package stockschecker.repositories

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import kirill5k.common.syntax.time.*
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.embedded.EmbeddedMongo
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.fixtures.*

import java.time.Instant
import scala.concurrent.Future
import scala.concurrent.duration.*

class StockRepositorySpec extends AsyncWordSpec with Matchers with EmbeddedMongo {

  "A StockRepository" when {
    "save" should {
      "store only 1 stock per day" in {
        withEmbeddedMongoDatabase { db =>
          val ts = Instant.parse("2020-01-01T00:00:00Z")
          for
            repo <- StockRepository.make(db)
            _    <- repo.save(AAPLStock.copy(lastUpdatedAt = ts))
            _    <- repo.save(AAPLStock.copy(lastUpdatedAt = ts.plus(1.hour)))
            _    <- repo.save(AAPLStock.copy(lastUpdatedAt = ts.plus(2.hour)))
            all  <- repo.streamAll.compile.toList
          yield all mustBe List(AAPLStock.copy(lastUpdatedAt = ts.plus(2.hour)))
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

      "find latest stock by ticker" in {
        withEmbeddedMongoDatabase { db =>
          val ts = Instant.parse("2020-01-01T00:00:00Z")
          for
            repo <- StockRepository.make(db)
            _    <- repo.save(AAPLStock.copy(lastUpdatedAt = ts))
            _    <- repo.save(AAPLStock.copy(lastUpdatedAt = ts.plus(1.day)))
            s    <- repo.find(AAPL)
          yield s mustBe Some(AAPLStock.copy(lastUpdatedAt = ts.plus(1.day)))
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
