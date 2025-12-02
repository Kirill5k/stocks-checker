package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.domain.{Exchange, SecurityKind, Ticker, TimePeriod}
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

      "filter by exchange" in {
        val nyseStock = MSFTSecurity.copy(ticker = Ticker("NYSE1"), exchange = Exchange.NYSE)
        val seedData = Map("securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity), toDoc(nyseStock)))
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(exchange = Some(Exchange.NASDAQ)), None)
          yield
            res.size mustBe 2
            res.forall(_.security.exchange == Exchange.NASDAQ) mustBe true
        }
      }

      "filter by kind" in {
        val etfStock = MSFTSecurity.copy(ticker = Ticker("SPY"), kind = SecurityKind.ETF)
        val seedData = Map("securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity), toDoc(etfStock)))
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(kind = Some(SecurityKind.Stock)), None)
          yield
            res.size mustBe 2
            res.forall(_.security.kind == SecurityKind.Stock) mustBe true
        }
      }

      "filter by country" in {
        val ukProfile = MSFTCompanyProfile.copy(ticker = Ticker("HSBA"), country = "GB")
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity), toDoc(MSFTSecurity.copy(ticker = Ticker("HSBA")))),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile), toDoc(ukProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(country = Some("US")), None)
          yield
            res.size mustBe 2
            res.forall(_.profile.exists(_.country == "US")) mustBe true
        }
      }

      "filter by minimum market cap" in {
        val smallCapProfile = MSFTCompanyProfile.copy(ticker = Ticker("SMALL"), marketCap = 1000000000L)
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity), toDoc(MSFTSecurity.copy(ticker = Ticker("SMALL")))),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile), toDoc(smallCapProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minMarketCap = Some(3000000000000L)), None)
          yield
            res.size mustBe 2
            res.forall(_.profile.exists(_.marketCap >= 3000000000000L)) mustBe true
        }
      }

      "filter by maximum market cap" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(maxMarketCap = Some(3200000000000L)), None)
          yield
            res.size mustBe 1
            res.forall(_.profile.exists(_.marketCap <= 3200000000000L)) mustBe true
        }
      }

      "filter by market cap range" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minMarketCap = Some(3000000000000L), maxMarketCap = Some(3200000000000L)), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe MSFT
        }
      }

      "filter by minimum price" in {
        val lowPriceSummary = AAPLPricePerformanceSummary.copy(ticker = Ticker("LOW"), latestPrice = BigDecimal("50.00"))
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity.copy(ticker = Ticker("LOW")))),
          "price-performance-summaries" -> List(toDoc(AAPLPricePerformanceSummary), toDoc(lowPriceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minPrice = Some(BigDecimal("200.00"))), None)
          yield
            res.size mustBe 1
            res.head.performanceSummary.exists(_.latestPrice >= BigDecimal("200.00")) mustBe true
        }
      }

      "filter by maximum price" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity)),
          "price-performance-summaries" -> List(toDoc(AAPLPricePerformanceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(maxPrice = Some(BigDecimal("300.00"))), None)
          yield
            res.size mustBe 1
            res.forall(_.performanceSummary.exists(_.latestPrice <= BigDecimal("300.00"))) mustBe true
        }
      }

      "filter by price range" in {
        val highPriceSummary = AAPLPricePerformanceSummary.copy(ticker = Ticker("HIGH"), latestPrice = BigDecimal("500.00"))
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity.copy(ticker = Ticker("HIGH")))),
          "price-performance-summaries" -> List(toDoc(AAPLPricePerformanceSummary), toDoc(highPriceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minPrice = Some(BigDecimal("200.00")), maxPrice = Some(BigDecimal("300.00"))), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe AAPL
        }
      }

      "filter by multiple criteria" in {
        val nyseStock = MSFTSecurity.copy(ticker = Ticker("NYSE1"), exchange = Exchange.NYSE)
        val etfStock = MSFTSecurity.copy(ticker = Ticker("SPY"), kind = SecurityKind.ETF)
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity), toDoc(nyseStock), toDoc(etfStock)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(
              StockFilters(
                exchange = Some(Exchange.NASDAQ),
                kind = Some(SecurityKind.Stock),
                country = Some("US"),
                minMarketCap = Some(3000000000000L)
              ),
              None
            )
          yield
            res.size mustBe 2
            res.forall(s =>
              s.security.exchange == Exchange.NASDAQ &&
              s.security.kind == SecurityKind.Stock &&
              s.profile.exists(p => p.country == "US" && p.marketCap >= 3000000000000L)
            ) mustBe true
        }
      }

      "filter by minimum price change with period" in {
        val goodPerformer = AAPLPricePerformanceSummary.copy(ticker = Ticker("GOOD"), oneYearChange = Some(BigDecimal("25.50")))
        val badPerformer = AAPLPricePerformanceSummary.copy(ticker = Ticker("BAD"), oneYearChange = Some(BigDecimal("5.00")))
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("GOOD"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("BAD")))
          ),
          "price-performance-summaries" -> List(toDoc(goodPerformer), toDoc(badPerformer))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minChange = Some(BigDecimal("20.00")), period = Some(TimePeriod.OneYear)), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("GOOD")
            res.head.performanceSummary.exists(_.oneYearChange.exists(_ >= BigDecimal("20.00"))) mustBe true
        }
      }

      "filter by maximum price change with period" in {
        val moderatePerformer = AAPLPricePerformanceSummary.copy(ticker = Ticker("MOD"), threeMonthChange = Some(BigDecimal("8.50")))
        val strongPerformer = AAPLPricePerformanceSummary.copy(ticker = Ticker("STRONG"), threeMonthChange = Some(BigDecimal("30.00")))
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("MOD"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("STRONG")))
          ),
          "price-performance-summaries" -> List(toDoc(moderatePerformer), toDoc(strongPerformer))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(maxChange = Some(BigDecimal("10.00")), period = Some(TimePeriod.ThreeMonth)), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("MOD")
            res.head.performanceSummary.exists(_.threeMonthChange.exists(_ <= BigDecimal("10.00"))) mustBe true
        }
      }

      "filter by price change range with period" in {
        val lowPerformer = AAPLPricePerformanceSummary.copy(ticker = Ticker("LOW"), oneMonthChange = Some(BigDecimal("2.00")))
        val midPerformer = AAPLPricePerformanceSummary.copy(ticker = Ticker("MID"), oneMonthChange = Some(BigDecimal("7.50")))
        val highPerformer = AAPLPricePerformanceSummary.copy(ticker = Ticker("HIGH"), oneMonthChange = Some(BigDecimal("15.00")))
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("LOW"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("MID"))),
            toDoc(AAPLSecurity.copy(ticker = Ticker("HIGH")))
          ),
          "price-performance-summaries" -> List(toDoc(lowPerformer), toDoc(midPerformer), toDoc(highPerformer))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(
              StockFilters(
                minChange = Some(BigDecimal("5.00")),
                maxChange = Some(BigDecimal("10.00")),
                period = Some(TimePeriod.OneMonth)
              ),
              None
            )
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("MID")
            res.head.performanceSummary.exists(p =>
              p.oneMonthChange.exists(c => c >= BigDecimal("5.00") && c <= BigDecimal("10.00"))
            ) mustBe true
        }
      }

      "not filter by change when period is not specified" in {
        val performer = AAPLPricePerformanceSummary.copy(ticker = Ticker("PERF"), oneYearChange = Some(BigDecimal("50.00")))
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity.copy(ticker = Ticker("PERF")))),
          "price-performance-summaries" -> List(toDoc(performer))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minChange = Some(BigDecimal("20.00"))), None)
          yield
            // Without period specified, minChange should not filter anything
            res.size mustBe 1
        }
      }

      "return empty list when filters don't match any stocks" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(exchange = Some(Exchange.NYSE)), None)
          yield res mustBe List.empty
        }
      }

      "respect limit when applying filters" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(exchange = Some(Exchange.NASDAQ)), Some(1))
          yield res.size mustBe 1
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

