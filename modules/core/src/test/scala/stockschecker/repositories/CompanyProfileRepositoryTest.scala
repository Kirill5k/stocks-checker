package stockschecker.repositories

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import mongo4cats.embedded.EmbeddedMongo
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.fixtures.{AAPL, AAPLCompanyProfile}

import scala.concurrent.Future

class CompanyProfileRepositoryTest extends AsyncWordSpec with Matchers with EmbeddedMongo {

  "A CompanyProfileRepository" when {
    "save" should {
      "save company profile in the repository" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.find(AAPL)
          yield res mustBe Some(AAPLCompanyProfile)
        }
      }
    }
  }

  def withEmbeddedMongoDatabase[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo(12147) {
      MongoClient
        .fromConnectionString[IO]("mongodb://localhost:12147")
        .evalMap(_.getDatabase("stock-checker"))
        .use(test)
    }.unsafeToFuture()(IORuntime.global)
}
