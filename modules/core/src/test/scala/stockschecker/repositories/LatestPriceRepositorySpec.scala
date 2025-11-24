package stockschecker.repositories

import cats.effect.IO
import stockschecker.fixtures.*

import java.time.LocalDate

class LatestPriceRepositorySpec extends RepositorySpec {

  override def port: Int = 12151

  "A LatestPriceRepository" when {
    "save" should {
      "save latest price in the repository" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            res  <- repo.find(AAPL, AAPLLatestPrice1.date)
          yield res mustBe Some(AAPLLatestPrice1)
        }
      }

      "save multiple prices for same ticker with different dates" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice2)
            _    <- repo.save(AAPLLatestPrice3)
            res1 <- repo.find(AAPL, AAPLLatestPrice1.date)
            res2 <- repo.find(AAPL, AAPLLatestPrice2.date)
            res3 <- repo.find(AAPL, AAPLLatestPrice3.date)
          yield
            res1 mustBe Some(AAPLLatestPrice1)
            res2 mustBe Some(AAPLLatestPrice2)
            res3 mustBe Some(AAPLLatestPrice3)
        }
      }

      "update price when saving same ticker and date" in {
        withEmbeddedMongoDatabase { db =>
          val updatedPrice = AAPLLatestPrice1.copy(price = BigDecimal("235.00"))
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(updatedPrice)
            res  <- repo.find(AAPL, AAPLLatestPrice1.date)
          yield res.map(_.price) mustBe Some(BigDecimal("235.00"))
        }
      }

      "save batch of prices" in {
        withEmbeddedMongoDatabase { db =>
          val prices = List(AAPLLatestPrice1, AAPLLatestPrice2, MSFTLatestPrice1)
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(prices)
            res1 <- repo.find(AAPL, AAPLLatestPrice1.date)
            res2 <- repo.find(AAPL, AAPLLatestPrice2.date)
            res3 <- repo.find(MSFT, MSFTLatestPrice1.date)
          yield
            res1 mustBe Some(AAPLLatestPrice1)
            res2 mustBe Some(AAPLLatestPrice2)
            res3 mustBe Some(MSFTLatestPrice1)
        }
      }
    }

    "find" should {
      "return None when price does not exist for ticker and date" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            res  <- repo.find(MSFT, LocalDate.parse("2025-10-01"))
          yield res mustBe None
        }
      }

      "find saved price by ticker and date" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            res  <- repo.find(AAPL, AAPLLatestPrice1.date)
          yield
            res mustBe defined
            res.map(_.ticker) mustBe Some(AAPL)
            res.map(_.price) mustBe Some(BigDecimal("228.50"))
            res.map(_.date) mustBe Some(LocalDate.parse("2025-10-01"))
        }
      }

      "return None for different date" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            res  <- repo.find(AAPL, LocalDate.parse("2025-10-05"))
          yield res mustBe None
        }
      }
    }

    "findLatestByTicker" should {
      "return None when no prices exist for ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            res  <- repo.findLatestByTicker(MSFT)
          yield res mustBe None
        }
      }

      "return most recent price for ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice2)
            _    <- repo.save(AAPLLatestPrice3)
            res  <- repo.findLatestByTicker(AAPL)
          yield
            res mustBe Some(AAPLLatestPrice3)
            res.map(_.date) mustBe Some(LocalDate.parse("2025-10-03"))
            res.map(_.price) mustBe Some(BigDecimal("232.50"))
        }
      }

      "return correct price when saved out of order" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice3)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice2)
            res  <- repo.findLatestByTicker(AAPL)
          yield res mustBe Some(AAPLLatestPrice3)
        }
      }
    }

    "findAllByTicker" should {
      "return empty list when no prices exist for ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            res  <- repo.findAllByTicker(MSFT)
          yield res mustBe List.empty
        }
      }

      "return all prices for ticker sorted by date descending" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice3)
            _    <- repo.save(AAPLLatestPrice2)
            _    <- repo.save(MSFTLatestPrice1)
            res  <- repo.findAllByTicker(AAPL)
          yield
            res.length mustBe 3
            res mustBe List(AAPLLatestPrice3, AAPLLatestPrice2, AAPLLatestPrice1)
        }
      }

      "return only prices for specified ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice2)
            _    <- repo.save(MSFTLatestPrice1)
            res  <- repo.findAllByTicker(AAPL)
          yield
            res.length mustBe 2
            res.head.ticker mustBe AAPL
            res.last.ticker mustBe AAPL
        }
      }

      "return single price when only one exists" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            res  <- repo.findAllByTicker(AAPL)
          yield
            res.length mustBe 1
            res.head mustBe AAPLLatestPrice1
        }
      }
    }
  }
}

