package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.domain.{Stock, Ticker}
import stockschecker.fixtures.{AAPL, AAPLCompanyProfile, AAPLPricePerformanceSummary, AAPLSecurity, AAPLStock, MSFT, MSFTCompanyProfile, MSFTSecurity, MSFTStock}

class StockRepositorySpec extends RepositorySpec {

  override def port: Int = 12148

  "A StockRepository" when {
    "find" should {
      "return stock with all associated data" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo    <- SecurityRepository.make[IO](db)
            profileRepo     <- CompanyProfileRepository.make[IO](db)
            performanceRepo <- PricePerformanceSummaryRepository.make[IO](db)
            stockRepo       <- StockRepository.make[IO](db)
            _               <- securityRepo.save(AAPLSecurity)
            _               <- profileRepo.save(AAPLCompanyProfile)
            _               <- performanceRepo.save(AAPLPricePerformanceSummary)
            res             <- stockRepo.find(AAPL)
          yield res mustBe Some(AAPLStock)
        }
      }

      "return stock with only security data when profile and performance don't exist" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo <- SecurityRepository.make[IO](db)
            stockRepo    <- StockRepository.make[IO](db)
            _            <- securityRepo.save(MSFTSecurity)
            res          <- stockRepo.find(MSFT)
          yield res mustBe Some(MSFTStock.copy(profile = None, performanceSummary = None))
        }
      }

      "return stock with partial data when only profile exists" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo <- SecurityRepository.make[IO](db)
            profileRepo  <- CompanyProfileRepository.make[IO](db)
            stockRepo    <- StockRepository.make[IO](db)
            _            <- securityRepo.save(MSFTSecurity)
            _            <- profileRepo.save(MSFTCompanyProfile)
            res          <- stockRepo.find(MSFT)
          yield res mustBe Some(MSFTStock.copy(performanceSummary = None))
        }
      }

      "return None when ticker doesn't exist" in {
        withEmbeddedMongoDatabase { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(AAPL)
          yield res mustBe None
        }
      }
    }

    "findAll" should {
      "return all stocks with associated data" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo    <- SecurityRepository.make[IO](db)
            profileRepo     <- CompanyProfileRepository.make[IO](db)
            performanceRepo <- PricePerformanceSummaryRepository.make[IO](db)
            stockRepo       <- StockRepository.make[IO](db)
            _               <- securityRepo.save(AAPLSecurity)
            _               <- securityRepo.save(MSFTSecurity)
            _               <- profileRepo.save(AAPLCompanyProfile)
            _               <- profileRepo.save(MSFTCompanyProfile)
            _               <- performanceRepo.save(AAPLPricePerformanceSummary)
            res             <- stockRepo.findAll(None)
          yield res.size mustBe 2
        }
      }

      "return limited number of stocks when limit is specified" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo <- SecurityRepository.make[IO](db)
            profileRepo  <- CompanyProfileRepository.make[IO](db)
            stockRepo    <- StockRepository.make[IO](db)
            _            <- securityRepo.save(AAPLSecurity)
            _            <- securityRepo.save(MSFTSecurity)
            _            <- profileRepo.save(AAPLCompanyProfile)
            _            <- profileRepo.save(MSFTCompanyProfile)
            res          <- stockRepo.findAll(Some(1))
          yield res.size mustBe 1
        }
      }

      "return empty list when no securities exist" in {
        withEmbeddedMongoDatabase { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(None)
          yield res mustBe List.empty
        }
      }

      "return all stocks when limit is greater than available stocks" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo <- SecurityRepository.make[IO](db)
            stockRepo    <- StockRepository.make[IO](db)
            _            <- securityRepo.save(AAPLSecurity)
            _            <- securityRepo.save(MSFTSecurity)
            res          <- stockRepo.findAll(Some(10))
          yield res.size mustBe 2
        }
      }
    }

    "findByTickers" should {
      "return stocks for all specified tickers" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo    <- SecurityRepository.make[IO](db)
            profileRepo     <- CompanyProfileRepository.make[IO](db)
            performanceRepo <- PricePerformanceSummaryRepository.make[IO](db)
            stockRepo       <- StockRepository.make[IO](db)
            _               <- securityRepo.save(AAPLSecurity)
            _               <- securityRepo.save(MSFTSecurity)
            _               <- profileRepo.save(AAPLCompanyProfile)
            _               <- profileRepo.save(MSFTCompanyProfile)
            _               <- performanceRepo.save(AAPLPricePerformanceSummary)
            res             <- stockRepo.findByTickers(NonEmptyList.of(AAPL, MSFT))
          yield res.size mustBe 2
        }
      }

      "return only existing stocks when some tickers don't exist" in {
        withEmbeddedMongoDatabase { db =>
          val nonExistent = Ticker("GOOG")
          for
            securityRepo <- SecurityRepository.make[IO](db)
            profileRepo  <- CompanyProfileRepository.make[IO](db)
            stockRepo    <- StockRepository.make[IO](db)
            _            <- securityRepo.save(AAPLSecurity)
            _            <- profileRepo.save(AAPLCompanyProfile)
            res          <- stockRepo.findByTickers(NonEmptyList.of(AAPL, nonExistent))
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe AAPL
        }
      }

      "return stock for single ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo    <- SecurityRepository.make[IO](db)
            profileRepo     <- CompanyProfileRepository.make[IO](db)
            performanceRepo <- PricePerformanceSummaryRepository.make[IO](db)
            stockRepo       <- StockRepository.make[IO](db)
            _               <- securityRepo.save(AAPLSecurity)
            _               <- profileRepo.save(AAPLCompanyProfile)
            _               <- performanceRepo.save(AAPLPricePerformanceSummary)
            res             <- stockRepo.findByTickers(NonEmptyList.of(AAPL))
          yield
            res.size mustBe 1
            res.head mustBe AAPLStock
        }
      }

      "return empty list when no tickers exist" in {
        withEmbeddedMongoDatabase { db =>
          val nonExistent1 = Ticker("GOOG")
          val nonExistent2 = Ticker("AMZN")
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findByTickers(NonEmptyList.of(nonExistent1, nonExistent2))
          yield res mustBe List.empty
        }
      }

      "return stocks with partial data when profile or performance missing" in {
        withEmbeddedMongoDatabase { db =>
          for
            securityRepo <- SecurityRepository.make[IO](db)
            profileRepo  <- CompanyProfileRepository.make[IO](db)
            stockRepo    <- StockRepository.make[IO](db)
            _            <- securityRepo.save(AAPLSecurity)
            _            <- securityRepo.save(MSFTSecurity)
            _            <- profileRepo.save(AAPLCompanyProfile)
            res          <- stockRepo.findByTickers(NonEmptyList.of(AAPL, MSFT))
          yield
            res.size mustBe 2
            res.exists(s => s.security.ticker == AAPL && s.profile.isDefined) mustBe true
            res.exists(s => s.security.ticker == MSFT && s.profile.isEmpty) mustBe true
        }
      }
    }
  }
}

