package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.domain.{Exchange, SecurityKind, StockFilters, Ticker, TimePeriod}
import stockschecker.fixtures.{AAPL, AAPLCompanyProfile, AAPLFinancialMetrics, AAPLPriceAnalytics, AAPLSecurity, AAPLStock, MSFT, MSFTCompanyProfile, MSFTFinancialMetrics, MSFTSecurity, MSFTStock}

class StockRepositorySpec extends RepositorySpec {

  override def port: Int = 12150

  "A StockRepository" when {
    "find" should {
      "return stock with all associated data" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity)),
          "company-profiles" -> List(toDoc(AAPLCompanyProfile)),
          "price-analytics" -> List(toDoc(AAPLPriceAnalytics)),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics))
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
          yield res mustBe Some(MSFTStock.copy(profile = None, priceAnalytics = None, financialMetrics = None))
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
          yield res mustBe Some(MSFTStock.copy(priceAnalytics = None, financialMetrics = None))
        }
      }

      "return stock with financial metrics when only financial metrics exist" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity)),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(AAPL)
          yield res mustBe Some(AAPLStock.copy(profile = None, priceAnalytics = None))
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
          "price-analytics" -> List(toDoc(AAPLPriceAnalytics)),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics), toDoc(MSFTFinancialMetrics))
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
        val lowPriceSummary = AAPLPriceAnalytics.copy(
          ticker = Ticker("LOW"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(latestPrice = BigDecimal("50.00"))
        )
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity.copy(ticker = Ticker("LOW")))),
          "price-analytics" -> List(toDoc(AAPLPriceAnalytics), toDoc(lowPriceSummary))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minPrice = Some(BigDecimal("200.00"))), None)
          yield
            res.size mustBe 1
            res.head.priceAnalytics.exists(_.performanceSummary.latestPrice >= BigDecimal("200.00")) mustBe true
        }
      }

      "filter by maximum price" in {
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity)),
          "price-analytics" -> List(toDoc(AAPLPriceAnalytics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(maxPrice = Some(BigDecimal("300.00"))), None)
          yield
            res.size mustBe 1
            res.forall(_.priceAnalytics.exists(_.performanceSummary.latestPrice <= BigDecimal("300.00"))) mustBe true
        }
      }

      "filter by price range" in {
        val highPriceSummary = AAPLPriceAnalytics.copy(
          ticker = Ticker("HIGH"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(latestPrice = BigDecimal("500.00"))
        )
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity.copy(ticker = Ticker("HIGH")))),
          "price-analytics" -> List(toDoc(AAPLPriceAnalytics), toDoc(highPriceSummary))
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
        val goodPerformer = AAPLPriceAnalytics.copy(
          ticker = Ticker("GOOD"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(oneYearChange = Some(BigDecimal("25.50")))
        )
        val badPerformer = AAPLPriceAnalytics.copy(
          ticker = Ticker("BAD"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(oneYearChange = Some(BigDecimal("5.00")))
        )
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("GOOD"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("BAD")))
          ),
          "price-analytics" -> List(toDoc(goodPerformer), toDoc(badPerformer))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minChange = Some(BigDecimal("20.00")), period = Some(TimePeriod.OneYear)), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("GOOD")
            res.head.priceAnalytics.exists(_.performanceSummary.oneYearChange.exists(_ >= BigDecimal("20.00"))) mustBe true
        }
      }

      "filter by maximum price change with period" in {
        val moderatePerformer = AAPLPriceAnalytics.copy(
          ticker = Ticker("MOD"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(threeMonthChange = Some(BigDecimal("8.50")))
        )
        val strongPerformer = AAPLPriceAnalytics.copy(
          ticker = Ticker("STRONG"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(threeMonthChange = Some(BigDecimal("30.00")))
        )
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("MOD"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("STRONG")))
          ),
          "price-analytics" -> List(toDoc(moderatePerformer), toDoc(strongPerformer))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(maxChange = Some(BigDecimal("10.00")), period = Some(TimePeriod.ThreeMonth)), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("MOD")
            res.head.priceAnalytics.exists(_.performanceSummary.threeMonthChange.exists(_ <= BigDecimal("10.00"))) mustBe true
        }
      }

      "filter by price change range with period" in {
        val lowPerformer = AAPLPriceAnalytics.copy(
          ticker = Ticker("LOW"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(oneMonthChange = Some(BigDecimal("2.00")))
        )
        val midPerformer = AAPLPriceAnalytics.copy(
          ticker = Ticker("MID"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(oneMonthChange = Some(BigDecimal("7.50")))
        )
        val highPerformer = AAPLPriceAnalytics.copy(
          ticker = Ticker("HIGH"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(oneMonthChange = Some(BigDecimal("15.00")))
        )
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("LOW"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("MID"))),
            toDoc(AAPLSecurity.copy(ticker = Ticker("HIGH")))
          ),
          "price-analytics" -> List(toDoc(lowPerformer), toDoc(midPerformer), toDoc(highPerformer))
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
            res.head.priceAnalytics.exists(p =>
              p.performanceSummary.oneMonthChange.exists(c => c >= BigDecimal("5.00") && c <= BigDecimal("10.00"))
            ) mustBe true
        }
      }

      "not filter by change when period is not specified" in {
        val performer = AAPLPriceAnalytics.copy(
          ticker = Ticker("PERF"),
          performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(oneYearChange = Some(BigDecimal("50.00")))
        )
        val seedData = Map(
          "securities" -> List(toDoc(AAPLSecurity.copy(ticker = Ticker("PERF")))),
          "price-analytics" -> List(toDoc(performer))
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

      "filter by maximum PE ratio" in {
        val highPEMetrics = AAPLFinancialMetrics.copy(
          ticker = Ticker("HIGHPE"),
          peRatioTtm = Some(BigDecimal(50.00))
        )
        val lowPEMetrics = MSFTFinancialMetrics.copy(
          ticker = Ticker("LOWPE"),
          peRatioTtm = Some(BigDecimal(25.00))
        )
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("HIGHPE"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("LOWPE")))
          ),
          "financial-metrics" -> List(toDoc(highPEMetrics), toDoc(lowPEMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(maxPE = Some(BigDecimal(30.00))), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("LOWPE")
            res.head.financialMetrics.exists(_.peRatioTtm.exists(_ <= BigDecimal(30.00))) mustBe true
        }
      }

      "filter by minimum ROE" in {
        val highROEMetrics = AAPLFinancialMetrics.copy(
          ticker = Ticker("HIGHROE"),
          roeTtm = Some(BigDecimal(150.00))
        )
        val lowROEMetrics = MSFTFinancialMetrics.copy(
          ticker = Ticker("LOWROE"),
          roeTtm = Some(BigDecimal(30.00))
        )
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("HIGHROE"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("LOWROE")))
          ),
          "financial-metrics" -> List(toDoc(highROEMetrics), toDoc(lowROEMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(minROE = Some(BigDecimal(100.00))), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("HIGHROE")
            res.head.financialMetrics.exists(_.roeTtm.exists(_ >= BigDecimal(100.00))) mustBe true
        }
      }

      "filter by maximum debt to equity" in {
        val highDebtMetrics = AAPLFinancialMetrics.copy(
          ticker = Ticker("HIGHDEBT"),
          debtToEquityAnnual = Some(BigDecimal(2.50))
        )
        val lowDebtMetrics = MSFTFinancialMetrics.copy(
          ticker = Ticker("LOWDEBT"),
          debtToEquityAnnual = Some(BigDecimal(0.50))
        )
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("HIGHDEBT"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("LOWDEBT")))
          ),
          "financial-metrics" -> List(toDoc(highDebtMetrics), toDoc(lowDebtMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(StockFilters(maxDebtToEquity = Some(BigDecimal(1.00))), None)
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("LOWDEBT")
            res.head.financialMetrics.exists(_.debtToEquityAnnual.exists(_ <= BigDecimal(1.00))) mustBe true
        }
      }

      "filter by multiple financial metrics criteria" in {
        val goodMetrics = AAPLFinancialMetrics.copy(
          ticker = Ticker("GOOD"),
          peRatioTtm = Some(BigDecimal(30.00)),
          roeTtm = Some(BigDecimal(120.00)),
          debtToEquityAnnual = Some(BigDecimal(0.80))
        )
        val badPEMetrics = MSFTFinancialMetrics.copy(
          ticker = Ticker("BADPE"),
          peRatioTtm = Some(BigDecimal(60.00)),
          roeTtm = Some(BigDecimal(120.00)),
          debtToEquityAnnual = Some(BigDecimal(0.80))
        )
        val badROEMetrics = AAPLFinancialMetrics.copy(
          ticker = Ticker("BADROE"),
          peRatioTtm = Some(BigDecimal(30.00)),
          roeTtm = Some(BigDecimal(20.00)),
          debtToEquityAnnual = Some(BigDecimal(0.80))
        )
        val seedData = Map(
          "securities" -> List(
            toDoc(AAPLSecurity.copy(ticker = Ticker("GOOD"))),
            toDoc(MSFTSecurity.copy(ticker = Ticker("BADPE"))),
            toDoc(AAPLSecurity.copy(ticker = Ticker("BADROE")))
          ),
          "financial-metrics" -> List(toDoc(goodMetrics), toDoc(badPEMetrics), toDoc(badROEMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.findAll(
              StockFilters(
                maxPE = Some(BigDecimal(40.00)),
                minROE = Some(BigDecimal(100.00)),
                maxDebtToEquity = Some(BigDecimal(1.00))
              ),
              None
            )
          yield
            res.size mustBe 1
            res.head.security.ticker mustBe Ticker("GOOD")
            res.head.financialMetrics.exists(m =>
              m.peRatioTtm.exists(_ <= BigDecimal(40.00)) &&
              m.roeTtm.exists(_ >= BigDecimal(100.00)) &&
              m.debtToEquityAnnual.exists(_ <= BigDecimal(1.00))
            ) mustBe true
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
          "price-analytics" -> List(toDoc(AAPLPriceAnalytics)),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics), toDoc(MSFTFinancialMetrics))
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
          "price-analytics" -> List(toDoc(AAPLPriceAnalytics)),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics))
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

