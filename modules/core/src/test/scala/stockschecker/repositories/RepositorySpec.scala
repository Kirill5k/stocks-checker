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
import stockschecker.domain.{CompanyProfile, FinancialMetrics, PriceAnalytics, Security}

import java.time.Instant
import scala.concurrent.Future

trait RepositorySpec extends AsyncWordSpec with Matchers with EmbeddedMongo {

  def port: Int

  private val now = Instant.now()

  protected def toDoc(security: Security): Document =
    Document(
      "_id" := security.ticker.value,
      "ticker" := security.ticker.value,
      "exchange" := security.exchange.print,
      "name" := security.name,
      "kind" := security.kind.print,
      "isActive" := security.isActive,
      "companyProfileLastUpdatedAt" := security.companyProfileLastUpdatedAt,
      "createdAt" := now,
      "updatedAt" := now
    )

  protected def toDoc(profile: CompanyProfile): Document =
    Document(
      "_id" := profile.ticker.value,
      "name" := profile.name,
      "country" := profile.country,
      "industry" := profile.industry,
      "description" := profile.description,
      "website" := profile.website,
      "ipoDate" := profile.ipoDate,
      "currency" := profile.currency,
      "marketCap" := profile.marketCap,
      "priceAnalyticsLastUpdatedAt" := profile.priceAnalyticsLastUpdatedAt,
      "financialMetricsLastUpdatedAt" := profile.financialMetricsLastUpdatedAt,
      "createdAt" := now,
      "updatedAt" := now
    )

  protected def toDoc(analytics: PriceAnalytics): Document =
    Document(
      "_id" := analytics.ticker,
      "performanceSummary" := analytics.performanceSummary,
      "metrics" := analytics.metrics,
      "scores" := analytics.scores,
      "createdAt" := now,
      "updatedAt" := now
    )

  protected def toDoc(metrics: FinancialMetrics): Document =
    Document(
      "_id" := metrics.ticker.value,
      "peRatioTtm" := metrics.peRatioTtm,
      "epsTtm" := metrics.epsTtm,
      "roeTtm" := metrics.roeTtm,
      "dividendYieldAnnual" := metrics.dividendYieldAnnual,
      "debtToEquityAnnual" := metrics.debtToEquityAnnual,
      "profitMarginTtm" := metrics.profitMarginTtm,
      "freeCashFlowPerShareTtm" := metrics.freeCashFlowPerShareTtm,
      "revenueGrowth5Y" := metrics.revenueGrowth5Y,
      "epsGrowth5Y" := metrics.epsGrowth5Y,
      "priceHigh52Week" := metrics.priceHigh52Week,
      "priceLow52Week" := metrics.priceLow52Week,
      "createdAt" := now,
      "updatedAt" := now
    )

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
