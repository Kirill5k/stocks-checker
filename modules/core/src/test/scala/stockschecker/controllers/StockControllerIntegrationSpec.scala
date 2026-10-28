package stockschecker.controllers

import cats.effect.IO
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.parser.parse
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.*
import org.http4s.{Header, Request, Status, Uri}
import org.http4s.implicits.*
import org.http4s.server.Router
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.fixtures.*
import stockschecker.actions.ActionDispatcher
import stockschecker.repositories.{JobRepository, RepositorySpec, StockRepository}
import stockschecker.services.StockService

import java.time.Instant

class StockControllerIntegrationSpec extends RepositorySpec {
  override def port: Int = 12161
  private val config     = ApiConfig("integration-api-key")
  private val authHeader = Header.Raw(CIString("X-API-Key"), config.key)

  "Stock query HTTP contract" should {
    List("missing", "null").foreach { basisState =>
      s"return financial metrics and price analytics when stored basis is $basisState" in {
        val analytics =
          if basisState == "null" then toDoc(AAPLPriceAnalytics.copy(basis = None))
          else
            Document(
              "_id"                := AAPL,
              "performanceSummary" := AAPLPriceAnalytics.performanceSummary,
              "metrics"            := AAPLPriceAnalytics.metrics,
              "scores"             := AAPLPriceAnalytics.scores,
              "createdAt"          := Instant.parse("2026-10-01T00:00:00Z"),
              "updatedAt"          := Instant.parse("2026-10-01T00:00:00Z")
            )
        val seed = Map(
          "securities"        -> List(toDoc(AAPLSecurity)),
          "company-profiles"  -> List(toDoc(AAPLCompanyProfile)),
          "price-analytics"   -> List(analytics),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo       <- StockRepository.make[IO](db)
            jobs       <- JobRepository.make[IO](db)
            dispatcher <- ActionDispatcher.make[IO](jobs)
            service    <- StockService.make(repo, dispatcher)
            controller <- StockController.make(service, config)
            response   <- Router("/api" -> controller.routes).orNotFound.run(
              Request[IO](uri = uri"/api/stocks/AAPL").withHeaders(authHeader)
            )
            body <- response.as[String]
            json <- IO.fromEither(parse(body))
          yield {
            response.status mustBe Status.Ok
            json.hcursor.get[String]("ticker") mustBe Right("AAPL")
            json.hcursor.downField("priceAnalytics").downField("basis").focus mustBe Some(Json.Null)
            json.hcursor.downField("priceAnalytics").downField("performanceSummary").get[BigDecimal]("latestPrice") mustBe
              Right(AAPLPricePerformanceSummary.latestPrice)
            json.hcursor.downField("financialMetrics").get[Option[BigDecimal]]("peRatioTtm") mustBe Right(AAPLFinancialMetrics.peRatioTtm)
            json.hcursor.downField("financialMetrics").get[Option[BigDecimal]]("epsTtm") mustBe Right(AAPLFinancialMetrics.epsTtm)
          }
        }
      }
    }

    "filter normalized consistency scores through the real controller, service and Mongo query" in {
      val high = AAPLPriceAnalytics.copy(
        scores = AAPLPriceAnalytics.scores.copy(consistencyScore = 90),
        metrics = AAPLPriceAnalytics.metrics.copy(consistencyScore = Some(BigDecimal("0.10")))
      )
      val low = high.copy(
        ticker = MSFT,
        scores = high.scores.copy(consistencyScore = 40),
        metrics = high.metrics.copy(consistencyScore = Some(BigDecimal("0.99")))
      )
      val seed = Map("securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)), "price-analytics" -> List(toDoc(high), toDoc(low)))
      withEmbeddedMongoDatabase(seed) { db =>
        for
          repo       <- StockRepository.make[IO](db)
          jobs       <- JobRepository.make[IO](db)
          dispatcher <- ActionDispatcher.make[IO](jobs)
          service    <- StockService.make(repo, dispatcher)
          controller <- StockController.make(service, config)
          response   <- controller.routes.orNotFound.run(
            Request[IO](uri = Uri.unsafeFromString("/stocks?minConsistencyScore=70&limit=20")).withHeaders(authHeader)
          )
          body <- response.as[String]
        yield {
          response.status mustBe Status.Ok
          parse(body).toOption.get.asArray.get.map(_.hcursor.get[String]("ticker").toOption.get).toList mustBe List("AAPL")
        }
      }
    }

    "reject invalid bounds, score units, limits and a missing change period" in
      withEmbeddedMongoDatabase { db =>
        for
          repo       <- StockRepository.make[IO](db)
          jobs       <- JobRepository.make[IO](db)
          dispatcher <- ActionDispatcher.make[IO](jobs)
          service    <- StockService.make(repo, dispatcher)
          controller <- StockController.make(service, config)
          statuses   <- List(
            "minConsistencyScore=101",
            "minOverallScore=-1",
            "minPrice=10&maxPrice=5",
            "minPE=30&maxPE=10",
            "minMarketCap=20&maxMarketCap=10",
            "minChange=1",
            "limit=0",
            "limit=-1"
          ).traverse { query =>
            controller.routes.orNotFound
              .run(Request[IO](uri = Uri.unsafeFromString(s"/stocks?$query")).withHeaders(authHeader))
              .map(_.status)
          }
        yield statuses.distinct mustBe List(Status.UnprocessableContent)
      }
  }
}
