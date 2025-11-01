package stockschecker.repositories

import cats.effect.IO
import stockschecker.fixtures.{AAPL, AAPLPricePerformanceSummary, MSFT}

class PricePerformanceSummaryRepositorySpec extends RepositorySpec {

  override def port: Int = 12148

  "A PricePerformanceSummaryRepository" when {
    "save" should {
      "save price performance summary in the repository" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PricePerformanceSummaryRepository.make[IO](db)
            _    <- repo.save(AAPLPricePerformanceSummary)
            res  <- repo.find(AAPL)
          yield res mustBe Some(AAPLPricePerformanceSummary)
        }
      }

      "be able to save and update same price performance summary in the repository" in {
        withEmbeddedMongoDatabase { db =>
          val updatedSummary = AAPLPricePerformanceSummary.copy(latestPrice = BigDecimal("999.99"))
          for
            repo <- PricePerformanceSummaryRepository.make[IO](db)
            _    <- repo.save(AAPLPricePerformanceSummary)
            _    <- repo.save(AAPLPricePerformanceSummary)
            _    <- repo.save(updatedSummary)
            res  <- repo.find(AAPL)
          yield res.map(_.latestPrice) mustBe Some(BigDecimal("999.99"))
        }
      }

      "update performance metrics when saving existing ticker" in {
        withEmbeddedMongoDatabase { db =>
          val updatedSummary = AAPLPricePerformanceSummary.copy(
            oneMonthChange = Some(BigDecimal("0.15")),
            threeMonthChange = Some(BigDecimal("0.25"))
          )
          for
            repo <- PricePerformanceSummaryRepository.make[IO](db)
            _    <- repo.save(AAPLPricePerformanceSummary)
            _    <- repo.save(updatedSummary)
            res  <- repo.find(AAPL)
          yield
            res.flatMap(_.oneMonthChange) mustBe Some(BigDecimal("0.15"))
            res.flatMap(_.threeMonthChange) mustBe Some(BigDecimal("0.25"))
        }
      }
    }

    "find" should {
      "return None when price performance summary does not exist" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PricePerformanceSummaryRepository.make[IO](db)
            res  <- repo.find(MSFT)
          yield res mustBe None
        }
      }

      "find saved price performance summary by ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- PricePerformanceSummaryRepository.make[IO](db)
            _    <- repo.save(AAPLPricePerformanceSummary)
            res  <- repo.find(AAPL)
          yield
            res mustBe defined
            res.map(_.ticker) mustBe Some(AAPL)
            res.map(_.latestPrice) mustBe Some(AAPLPricePerformanceSummary.latestPrice)
        }
      }
    }
  }
}

