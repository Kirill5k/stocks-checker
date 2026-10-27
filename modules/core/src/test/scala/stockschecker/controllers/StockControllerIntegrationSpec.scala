package stockschecker.controllers

import cats.effect.IO
import cats.syntax.traverse.*
import io.circe.parser.parse
import org.http4s.{Header, Request, Status, Uri}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.fixtures.*
import stockschecker.repositories.{CompanyProfileRepository, RepositorySpec, SecurityRepository, StockRepository}
import stockschecker.services.StockService

class StockControllerIntegrationSpec extends RepositorySpec {
  override def port: Int = 12161
  private val config     = ApiConfig("integration-api-key")
  private val authHeader = Header.Raw(CIString("X-API-Key"), config.key)

  "Stock query HTTP contract" should {
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
          securities <- SecurityRepository.make[IO](db)
          profiles   <- CompanyProfileRepository.make[IO](db)
          service    <- StockService.make(repo, securities, profiles)
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
          securities <- SecurityRepository.make[IO](db)
          profiles   <- CompanyProfileRepository.make[IO](db)
          service    <- StockService.make(repo, securities, profiles)
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
