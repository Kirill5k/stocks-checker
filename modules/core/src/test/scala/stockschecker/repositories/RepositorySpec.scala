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
import mongo4cats.operations.Filter
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.domain.{CompanyProfile, FinancialMetrics, PriceAnalytics, Security, Ticker}
import stockschecker.fixtures.{AAPLCompanyProfile, AAPLSecurity, MSFT, MSFTCompanyProfile, MSFTSecurity}
import stockschecker.repositories.entities.{CompanyProfileEntity, SecurityEntity}

import java.time.Instant
import scala.concurrent.Future

trait RepositorySpec extends AsyncWordSpec with Matchers with EmbeddedMongo {

  def port: Int

  private val now = Instant.now()

  protected def readSecurity(db: MongoDatabase[IO], ticker: Ticker): IO[Option[Security]] =
    db.getCollectionWithCodec[SecurityEntity](SecurityRepository.CollectionName)
      .flatMap(_.find(Filter.idEq(ticker.value)).first)
      .map(_.map(_.toDomain))

  protected def readProfile(db: MongoDatabase[IO], ticker: Ticker): IO[Option[CompanyProfile]] =
    db.getCollectionWithCodec[CompanyProfileEntity](CompanyProfileRepository.CollectionName)
      .flatMap(_.find(Filter.idEq(ticker.value)).first)
      .map(_.map(_.toDomain))

  protected def withActiveStocks[A](tickers: List[Ticker])(test: MongoDatabase[IO] => IO[A]): Future[A] =
    withActiveStocks(
      Map(
        SecurityRepository.CollectionName -> tickers.map { ticker =>
          toDoc(if ticker == MSFT then MSFTSecurity else AAPLSecurity.copy(ticker = ticker))
        },
        CompanyProfileRepository.CollectionName -> tickers.map { ticker =>
          toDoc(if ticker == MSFT then MSFTCompanyProfile else AAPLCompanyProfile.copy(ticker = ticker))
        }
      )
    )(test)

  protected def withActiveStocks[A](seed: Map[String, List[Document]])(test: MongoDatabase[IO] => IO[A]): Future[A] =
    val profiles = seed.getOrElse(CompanyProfileRepository.CollectionName, Nil)
    val existing = profiles.flatMap(_.getString("_id")).toSet
    val missing  = seed.getOrElse(SecurityRepository.CollectionName, Nil).flatMap(_.getString("_id")).filterNot(existing)
    val complete = profiles ++ missing.map(ticker => toDoc(AAPLCompanyProfile.copy(ticker = Ticker(ticker))))
    withEmbeddedMongoDatabase(seed.updated(CompanyProfileRepository.CollectionName, complete))(test)

  protected def toDoc(security: Security): Document =
    Document(
      "_id"                         := security.ticker.value,
      "ticker"                      := security.ticker.value,
      "exchange"                    := security.exchange.print,
      "name"                        := security.name,
      "kind"                        := security.kind.print,
      "isActive"                    := security.isActive,
      "listingStatus"               := security.listingStatus.print,
      "exclusionReason"             := security.exclusionReason,
      "companyProfileLastUpdatedAt" := security.companyProfileLastUpdatedAt,
      "createdAt"                   := now,
      "updatedAt"                   := now
    )

  protected def toDoc(profile: CompanyProfile): Document =
    Document(
      "_id"                           := profile.ticker.value,
      "isActive"                      := true,
      "name"                          := profile.name,
      "country"                       := profile.country,
      "industry"                      := profile.industry,
      "sector"                        := profile.sector,
      "description"                   := profile.description,
      "website"                       := profile.website,
      "ipoDate"                       := profile.ipoDate,
      "currency"                      := profile.currency,
      "marketCap"                     := profile.marketCap,
      "priceAnalyticsLastUpdatedAt"   := profile.priceAnalyticsLastUpdatedAt,
      "financialMetricsLastUpdatedAt" := profile.financialMetricsLastUpdatedAt,
      "createdAt"                     := now,
      "updatedAt"                     := now
    )

  protected def toDoc(analytics: PriceAnalytics): Document =
    Document(
      "_id"                := analytics.ticker,
      "performanceSummary" := analytics.performanceSummary,
      "metrics"            := analytics.metrics,
      "scores"             := analytics.scores,
      "basis"              := analytics.basis,
      "createdAt"          := now,
      "updatedAt"          := now
    )

  protected def toDoc(metrics: FinancialMetrics): Document =
    Document(
      "_id"                     := metrics.ticker.value,
      "peRatioTtm"              := metrics.peRatioTtm,
      "epsTtm"                  := metrics.epsTtm,
      "roeTtm"                  := metrics.roeTtm,
      "dividendYieldAnnual"     := metrics.dividendYieldAnnual,
      "debtToEquityAnnual"      := metrics.debtToEquityAnnual,
      "profitMarginTtm"         := metrics.profitMarginTtm,
      "freeCashFlowPerShareTtm" := metrics.freeCashFlowPerShareTtm,
      "revenueGrowth5Y"         := metrics.revenueGrowth5Y,
      "epsGrowth5Y"             := metrics.epsGrowth5Y,
      "priceHigh52Week"         := metrics.priceHigh52Week,
      "priceLow52Week"          := metrics.priceLow52Week,
      "createdAt"               := now,
      "updatedAt"               := now
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
