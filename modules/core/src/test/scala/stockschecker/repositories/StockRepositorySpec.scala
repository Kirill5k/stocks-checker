package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.domain.Ticker
import stockschecker.fixtures.{AAPL, AAPLCompanyProfile, AAPLPricePerformanceSummary, AAPLSecurity, AAPLStock, MSFT, MSFTCompanyProfile, MSFTSecurity, MSFTStock}

class StockRepositorySpec extends RepositorySpec {

  override def port: Int = 12150

  "A StockRepository" when {
    "find" should {
      "return stock with all associated data" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile)),
          "price-performance-summaries" -> List(toDoc(AAPLPricePerformanceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(AAPL)
          yield res mustBe Some(AAPLStock)
        }
      }

      "return stock with only security data when profile and performance don't exist" in {
        val seedData = Map("securities" -> List(toDoc(MSFTSecurity)))
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(MSFT)
          yield res mustBe Some(MSFTStock.copy(profile = None, performanceSummary = None))
        }
      }

      "return stock with partial data when only profile exists" in {
        val seedData = Map(
          "securities" -> List(toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(MSFT)
          yield res mustBe Some(MSFTStock.copy(performanceSummary = None))
        }
      }

      "return None when ticker doesn't exist" in {
        withEmbeddedMongoDatabase(Map.empty) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(AAPL)
          yield res mustBe None
        }
      }
    }

    "findAll" should {
      "return all stocks with associated data" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile)),
          "price-performance-summaries" -> List(toDoc(AAPLPricePerformanceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(), None)
          yield res.size mustBe 2
        }
      }

      "return limited number of stocks when limit is specified" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(), Some(1))
          yield res.size mustBe 1
        }
      }

      "return empty list when no securities exist" in {
        withEmbeddedMongoDatabase(Map.empty) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(), None)
          yield res mustBe List.empty
        }
      }

      "return all stocks when limit is greater than available stocks" in {
        val seedData = Map("securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)))
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(), Some(10))
          yield res.size mustBe 2
        }
      }
    }

    "findByTickers" should {
      "return stocks for all specified tickers" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile)),
          "price-performance-summaries" -> List(toDoc(AAPLPricePerformanceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findByTickers(NonEmptyList.of(AAPL, MSFT))
          yield res.size mustBe 2
        }
      }

      "return only existing stocks when some tickers don't exist" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          val nonExistent = Ticker("GOOG")
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findByTickers(NonEmptyList.of(AAPL, nonExistent))
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe AAPL
        }
      }

      "return stock for single ticker" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile)),
          "price-performance-summaries" -> List(toDoc(AAPLPricePerformanceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findByTickers(NonEmptyList.of(AAPL))
          yield
            res.size mustBe 1
            res.head mustBe AAPLStock
        }
      }

      "return empty list when no tickers exist" in {
        withEmbeddedMongoDatabase(Map.empty) { db =>
          val nonExistent1 = Ticker("GOOG")
          val nonExistent2 = Ticker("AMZN")
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findByTickers(NonEmptyList.of(nonExistent1, nonExistent2))
          yield res mustBe List.empty
        }
      }

      "return stocks with partial data when profile or performance missing" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findByTickers(NonEmptyList.of(AAPL, MSFT))
          yield
            res.size mustBe 2
            res.exists(s => s.security.ticker == AAPL && s.profile.isDefined) mustBe true
            res.exists(s => s.security.ticker == MSFT && s.profile.isEmpty) mustBe true
        }
      }
    }
  }
}

