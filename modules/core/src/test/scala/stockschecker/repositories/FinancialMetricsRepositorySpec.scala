package stockschecker.repositories

import cats.effect.IO
import stockschecker.domain.FinancialMetrics
import stockschecker.fixtures.*

class FinancialMetricsRepositorySpec extends RepositorySpec {

  override def port: Int = 12153

  "A FinancialMetricsRepository" when {
    "save" should {
      "save financial metrics in the repository" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- FinancialMetricsRepository.make[IO](db)
            _    <- repo.save(List(AAPLFinancialMetrics))
            res  <- repo.find(AAPL)
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
            res  <- repo.find(AAPL)
          yield res.map(_.peRatioTtm) mustBe Some(Some(BigDecimal(30.50)))
        }
    }

    "find" should {
      "return None when metrics do not exist for ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- FinancialMetricsRepository.make[IO](db)
            res  <- repo.find(MSFT)
          yield res mustBe None
        }

      "find metrics by ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- FinancialMetricsRepository.make[IO](db)
            _    <- repo.save(List(AAPLFinancialMetrics))
            res  <- repo.find(AAPL)
          yield
            res mustBe defined
            res.map(_.ticker) mustBe Some(AAPL)
            res.map(_.peRatioTtm) mustBe Some(Some(BigDecimal(34.11)))
        }
    }
  }
}
