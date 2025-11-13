package stockschecker.repositories

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.*
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.embedded.EmbeddedMongo
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.repositories.entities.{CompanyProfileEntity, PricePerformanceSummaryEntity, SecurityEntity}

import java.time.Instant
import scala.concurrent.Future

trait RepositorySpec extends AsyncWordSpec with Matchers with EmbeddedMongo {

  def port: Int

  private val now = Instant.now()

  protected def toDoc(security: stockschecker.domain.Security): Document =
    SecurityEntity.from(security, now).toBson.asDocument.get

  protected def toDoc(profile: stockschecker.domain.CompanyProfile): Document =
    CompanyProfileEntity.from(profile, now).toBson.asDocument.get

  protected def toDoc(summary: stockschecker.domain.PricePerformanceSummary): Document =
    PricePerformanceSummaryEntity.from(summary, now).toBson.asDocument.get

  protected def withEmbeddedMongoDatabase[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo(port) {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:${port}")
        .evalMap(_.getDatabase("stock-checker"))
        .use(test)
    }.unsafeToFuture()(using IORuntime.global)

  protected def withEmbeddedMongoDatabase[A](
      seedData: Map[String, List[Document]]
  )(test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo(port) {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:${port}")
        .evalMap(_.getDatabase("stock-checker"))
        .use { db =>
          for
            _ <- seedData.toList.traverse { case (collectionName, documents) =>
              db.getCollection(collectionName).flatMap { collection =>
                IO.whenA(documents.nonEmpty)(collection.insertMany(documents).void)
              }
            }
            result <- test(db)
          yield result
        }
    }.unsafeToFuture()(using IORuntime.global)
}
