package stockchecker.repositories

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.embedded.EmbeddedMongo
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import stockchecker.fixtures.*

import scala.concurrent.Future

class StockRepositorySpec extends AsyncWordSpec with Matchers with EmbeddedMongo {

  "A StockRepository" when {
    "save" should {
      "store single stock in database" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- StockRepository.make(db)
            _    <- repo.save(AAPLSock)
            all  <- repo.streamAll.compile.toList
          yield all mustBe List(AAPLSock)
        }
      }

      "store multiple stocks in database" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- StockRepository.make(db)
            stocks = List(AAPLSock, AAPLSock, AAPLSock)
            _   <- repo.save(stocks)
            all <- repo.streamAll.compile.toList
          yield all mustBe stocks
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
