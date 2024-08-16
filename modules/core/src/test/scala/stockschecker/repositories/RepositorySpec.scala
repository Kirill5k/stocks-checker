package stockschecker.repositories

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.embedded.EmbeddedMongo
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future

trait RepositorySpec extends AsyncWordSpec with Matchers with EmbeddedMongo  {

  def port: Int
  
  protected def withEmbeddedMongoDatabase[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo(port) {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:${port}")
        .evalMap(_.getDatabase("stock-checker"))
        .use(test)
    }.unsafeToFuture()(IORuntime.global)
}
