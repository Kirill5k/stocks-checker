package stockschecker.repositories

import cats.effect.IO
import mongo4cats.database.MongoDatabase
import mongo4cats.circe.*
import mongo4cats.operations.Filter
import stockschecker.domain.{FinancialMetrics, Ticker}
import stockschecker.repositories.entities.FinancialMetricsEntity
import stockschecker.fixtures.*

class FinancialMetricsRepositorySpec extends RepositorySpec {

  override def port: Int = 12153

  "A FinancialMetricsRepository" when {
    "delete" should {
      "remove only the requested metrics and remain idempotent" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- FinancialMetricsRepository.make[IO](db)
            _        <- repo.save(List(AAPLFinancialMetrics, MSFTFinancialMetrics))
            _        <- repo.delete(AAPL)
            _        <- repo.delete(AAPL)
            removed  <- read(db, AAPL)
            retained <- read(db, MSFT)
          yield
            removed mustBe None
            retained mustBe Some(MSFTFinancialMetrics)
        }
    }
    "save" should {
      "save financial metrics in the repository" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- FinancialMetricsRepository.make[IO](db)
            _    <- repo.save(List(AAPLFinancialMetrics))
            res  <- read(db, AAPL)
          yield res mustBe Some(AAPLFinancialMetrics)
        }

      "update metrics when saving same ticker" in
        withEmbeddedMongoDatabase { db =>
          val updatedMetrics = AAPLFinancialMetrics.copy(
            peRatioTtm = Some(BigDecimal(30.50))
          )
          for
            repo <- FinancialMetricsRepository.make[IO](db)
            _    <- repo.save(List(AAPLFinancialMetrics))
            _    <- repo.save(List(updatedMetrics))
            res  <- read(db, AAPL)
          yield res.map(_.peRatioTtm) mustBe Some(Some(BigDecimal(30.50)))
        }
    }

  }

  private def read(db: MongoDatabase[IO], ticker: Ticker): IO[Option[FinancialMetrics]] =
    db.getCollectionWithCodec[FinancialMetricsEntity](FinancialMetricsRepository.CollectionName)
      .flatMap(_.find(Filter.idEq(ticker.value)).first)
      .map(_.map(_.toDomain))
}
