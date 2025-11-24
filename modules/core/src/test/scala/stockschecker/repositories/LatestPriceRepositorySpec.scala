package stockschecker.repositories

import cats.effect.IO
import stockschecker.fixtures.*

import java.time.LocalDate

class LatestPriceRepositorySpec extends RepositorySpec {

  override def port: Int = 12151

  "A LatestPriceRepository" when {
    "save" should {
      "save latest price in the repository" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            res  <- repo.findLatest(AAPL)
          yield res mustBe Some(AAPLLatestPrice1)
        }

      "save multiple prices for same ticker with different dates" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice2)
            _    <- repo.save(AAPLLatestPrice3)
            res  <- repo.findLatest(AAPL)
          yield res mustBe Some(AAPLLatestPrice3)
        }

      "update price when saving same ticker and date" in
        withEmbeddedMongoDatabase { db =>
          val updatedPrice = AAPLLatestPrice1.copy(price = BigDecimal("235.00"))
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(updatedPrice)
            res  <- repo.findLatest(AAPL)
          yield res.map(_.price) mustBe Some(BigDecimal("235.00"))
        }

      "save batch of prices" in
        withEmbeddedMongoDatabase { db =>
          val prices = List(AAPLLatestPrice1, AAPLLatestPrice2, MSFTLatestPrice1)
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(prices)
            res1 <- repo.findLatest(AAPL)
            res2 <- repo.findLatest(MSFT)
          yield
            res1 mustBe Some(AAPLLatestPrice2)
            res2 mustBe Some(MSFTLatestPrice1)
        }
    }

    "find" should {
      "return None when price does not exist for ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            res  <- repo.findLatest(MSFT)
          yield res mustBe None
        }

      "find latest price by ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            res  <- repo.findLatest(AAPL)
          yield
            res mustBe defined
            res.map(_.ticker) mustBe Some(AAPL)
            res.map(_.price) mustBe Some(BigDecimal("228.50"))
            res.map(_.date) mustBe Some(LocalDate.parse("2025-10-01"))
        }

      "return latest price when multiple dates exist" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice3)
            _    <- repo.save(AAPLLatestPrice2)
            res  <- repo.findLatest(AAPL)
          yield
            res mustBe Some(AAPLLatestPrice3)
            res.map(_.date) mustBe Some(LocalDate.parse("2025-10-03"))
        }
    }

    "findAllByTicker" should {
      "return empty list when no prices exist for ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            res  <- repo.getAll(MSFT)
          yield res mustBe List.empty
        }

      "return all prices for ticker sorted by date descending" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice3)
            _    <- repo.save(AAPLLatestPrice2)
            _    <- repo.save(MSFTLatestPrice1)
            res  <- repo.getAll(AAPL)
          yield
            res.length mustBe 3
            res mustBe List(AAPLLatestPrice3, AAPLLatestPrice2, AAPLLatestPrice1)
        }

      "return only prices for specified ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            _    <- repo.save(AAPLLatestPrice2)
            _    <- repo.save(MSFTLatestPrice1)
            res  <- repo.getAll(AAPL)
          yield
            res.length mustBe 2
            res.head.ticker mustBe AAPL
            res.last.ticker mustBe AAPL
        }

      "return single price when only one exists" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- LatestPriceRepository.make[IO](db)
            _    <- repo.save(AAPLLatestPrice1)
            res  <- repo.getAll(AAPL)
          yield
            res.length mustBe 1
            res.head mustBe AAPLLatestPrice1
        }
    }
  }
}
