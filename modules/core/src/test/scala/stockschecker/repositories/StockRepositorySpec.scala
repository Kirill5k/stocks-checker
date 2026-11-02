package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import mongo4cats.bson.{BsonValue, Document}
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.domain.{Exchange, ListingStatus, SecurityKind, StockFilters, StockSortField, Ticker, TimePeriod}
import stockschecker.fixtures.{
  AAPL,
  AAPLCompanyProfile,
  AAPLFinancialMetrics,
  AAPLPriceAnalytics,
  AAPLSecurity,
  AAPLStock,
  MSFT,
  MSFTCompanyProfile,
  MSFTSecurity,
  MSFTStock
}

class StockRepositorySpec extends RepositorySpec with MongoJsonCodecs {

  override def port: Int = 12150

  "A StockRepository" when {
    "exists" should {
      "find a security without any joined records and reject an absent ticker" in {
        val seed = Map(SecurityRepository.CollectionName -> List(toDoc(AAPLSecurity)))
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo    <- StockRepository.make[IO](db)
            present <- repo.exists(AAPL)
            absent  <- repo.exists(MSFT)
          yield
            present mustBe true
            absent mustBe false
        }
      }

      "read only the security ID without decoding stock fields or foreign collections" in {
        val idOnly = Document("_id" := AAPL.value)
        val seed   = Map(
          SecurityRepository.CollectionName         -> List(idOnly),
          CompanyProfileRepository.CollectionName   -> List(idOnly),
          PriceAnalyticsRepository.CollectionName   -> List(idOnly),
          FinancialMetricsRepository.CollectionName -> List(idOnly)
        )
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo   <- StockRepository.make[IO](db)
            exists <- repo.exists(AAPL)
          yield exists mustBe true
        }
      }

      "ignore foreign records when the security does not exist" in {
        val seed = Map(
          CompanyProfileRepository.CollectionName   -> List(toDoc(AAPLCompanyProfile)),
          PriceAnalyticsRepository.CollectionName   -> List(toDoc(AAPLPriceAnalytics)),
          FinancialMetricsRepository.CollectionName -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo   <- StockRepository.make[IO](db)
            exists <- repo.exists(AAPL)
          yield exists mustBe false
        }
      }
    }

    "find" should {
      "retain joined analytics with missing or null basis" in {
        val msftAnalytics = AAPLPriceAnalytics.copy(ticker = MSFT)
        val aapl          = AAPLStock.copy(priceAnalytics = Some(AAPLPriceAnalytics.copy(basis = None)))
        val msft          = MSFTStock.copy(
          profile = MSFTCompanyProfile,
          priceAnalytics = Some(msftAnalytics.copy(basis = None)),
          financialMetrics = None
        )
        val seedData = Map(
          SecurityRepository.CollectionName       -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          CompanyProfileRepository.CollectionName -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile)),
          PriceAnalyticsRepository.CollectionName -> List(
            toDoc(AAPLPriceAnalytics).remove("basis"),
            toDoc(msftAnalytics).add("basis", BsonValue.Null)
          ),
          FinancialMetricsRepository.CollectionName -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            repo      <- StockRepository.make[IO](db)
            foundAapl <- repo.find(AAPL)
            foundMsft <- repo.find(MSFT)
          yield
            foundAapl mustBe Some(aapl)
            foundMsft mustBe Some(msft)
        }
      }

      "return stock with all associated data" in {
        val seedData = Map(
          "securities"        -> List(toDoc(AAPLSecurity)),
          "company-profiles"  -> List(toDoc(AAPLCompanyProfile)),
          "price-analytics"   -> List(toDoc(AAPLPriceAnalytics)),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(AAPL)
          yield res mustBe Some(AAPLStock)
        }
      }

      "hide a stock when no cached company profile exists" in {
        val seedData = Map("securities" -> List(toDoc(MSFTSecurity)))
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(MSFT)
          yield res mustBe None
        }
      }

      "return stock with partial data when only profile exists" in {
        val seedData = Map(
          "securities"       -> List(toDoc(MSFTSecurity)),
          "company-profiles" -> List(toDoc(MSFTCompanyProfile))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(MSFT)
          yield res mustBe Some(MSFTStock.copy(priceAnalytics = None, financialMetrics = None))
        }
      }

      "hide financial metrics when the company profile is missing" in {
        val seedData = Map(
          "securities"        -> List(toDoc(AAPLSecurity)),
          "financial-metrics" -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(AAPL)
          yield res mustBe None
        }
      }

      "return None when ticker doesn't exist" in
        withEmbeddedMongoDatabase(Map.empty) { db =>
          for
            stockRepo <- StockRepository.make[IO](db)
            res       <- stockRepo.find(AAPL)
          yield res mustBe None
        }
    }

    "findAll" should {
      "require an active listed security and profile before sorting and limiting" in {
        val inactive       = Ticker("INACTIVE")
        val delisted       = Ticker("DELISTED")
        val missingProfile = Ticker("MISSING")
        val hiddenTickers  = List(inactive, delisted, missingProfile)
        val seed           = Map(
          SecurityRepository.CollectionName -> List(
            toDoc(AAPLSecurity),
            toDoc(AAPLSecurity.copy(ticker = inactive, isActive = false)),
            toDoc(AAPLSecurity.copy(ticker = delisted, listingStatus = ListingStatus.Delisted)),
            toDoc(AAPLSecurity.copy(ticker = missingProfile))
          ),
          CompanyProfileRepository.CollectionName -> (
            List(toDoc(AAPLCompanyProfile.copy(marketCap = 1))) ++
              List(inactive, delisted).map(ticker => toDoc(AAPLCompanyProfile.copy(ticker = ticker)))
          )
        )
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo    <- StockRepository.make[IO](db)
            limited <- repo.findAll(StockFilters(), Some(1))
            all     <- repo.findAll(StockFilters(), None)
            hidden  <- hiddenTickers.traverse(repo.find)
          yield
            limited.map(_.security.ticker) mustBe List(AAPL)
            all.map(_.security.ticker) mustBe List(AAPL)
            hidden mustBe List.fill(hiddenTickers.size)(None)
        }
      }

      "ignore obsolete profile activation fields" in {
        val seed = Map(
          SecurityRepository.CollectionName       -> List(toDoc(AAPLSecurity)),
          CompanyProfileRepository.CollectionName -> List(toDoc(AAPLCompanyProfile).add("isActive", false.toBson))
        )
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo   <- StockRepository.make[IO](db)
            stocks <- repo.findAll(StockFilters(), None)
          yield stocks.map(_.security.ticker) mustBe List(AAPL)
        }
      }

      "return empty results without securities" in withEmbeddedMongoDatabase { db =>
        for
          repo   <- StockRepository.make[IO](db)
          stocks <- repo.findAll(StockFilters(), None)
        yield stocks mustBe Nil
      }

      val low     = Ticker("LOW")
      val mid     = Ticker("MID")
      val high    = Ticker("HIGH")
      val missing = Ticker("MISSING_DATA")
      val ordered = List(high, mid, low, missing)
      val seed    = Map(
        SecurityRepository.CollectionName -> List(
          toDoc(AAPLSecurity.copy(ticker = low, exchange = Exchange.NYSE, kind = SecurityKind.ETF)),
          toDoc(AAPLSecurity.copy(ticker = mid)),
          toDoc(AAPLSecurity.copy(ticker = high)),
          toDoc(AAPLSecurity.copy(ticker = missing))
        ),
        CompanyProfileRepository.CollectionName -> List(
          toDoc(AAPLCompanyProfile.copy(ticker = low, country = "GB", marketCap = 1000000000L)),
          toDoc(AAPLCompanyProfile.copy(ticker = mid, country = "US", marketCap = 3100000000000L)),
          toDoc(AAPLCompanyProfile.copy(ticker = high, country = "US", marketCap = 3400000000000L)),
          toDoc(AAPLCompanyProfile.copy(ticker = missing, country = "CA", marketCap = 500L))
        ),
        PriceAnalyticsRepository.CollectionName -> List(
          (low, BigDecimal(50), BigDecimal(2), BigDecimal(20), BigDecimal(45)),
          (mid, BigDecimal(250), BigDecimal("7.5"), BigDecimal(60), BigDecimal(20)),
          (high, BigDecimal(500), BigDecimal(25), BigDecimal(90), BigDecimal(8))
        ).map { case (ticker, price, change, score, drawdown) =>
          toDoc(
            AAPLPriceAnalytics.copy(
              ticker = ticker,
              performanceSummary = AAPLPriceAnalytics.performanceSummary
                .copy(latestPrice = price, oneMonthChange = Some(change), threeMonthChange = Some(change), oneYearChange = Some(change)),
              metrics = AAPLPriceAnalytics.metrics.copy(maxDrawdown = Some(drawdown)),
              scores =
                AAPLPriceAnalytics.scores.copy(overallScore = score, cagrScore = score, volatilityScore = score, consistencyScore = score)
            )
          )
        },
        FinancialMetricsRepository.CollectionName -> List(
          (
            low,
            BigDecimal(12),
            BigDecimal(5),
            BigDecimal("2.5"),
            BigDecimal(5),
            BigDecimal(-2),
            BigDecimal(3),
            BigDecimal(5),
            BigDecimal("0.4")
          ),
          (
            mid,
            BigDecimal(25),
            BigDecimal(30),
            BigDecimal(1),
            BigDecimal(20),
            BigDecimal(8),
            BigDecimal(12),
            BigDecimal(18),
            BigDecimal(2)
          ),
          (
            high,
            BigDecimal(50),
            BigDecimal(150),
            BigDecimal("0.5"),
            BigDecimal(40),
            BigDecimal(15),
            BigDecimal(20),
            BigDecimal(30),
            BigDecimal(5)
          )
        ).map { case (ticker, pe, roe, debt, margin, cash, revenue, eps, dividend) =>
          toDoc(
            AAPLFinancialMetrics.copy(
              ticker = ticker,
              peRatioTtm = Some(pe),
              roeTtm = Some(roe),
              debtToEquityAnnual = Some(debt),
              profitMarginTtm = Some(margin),
              freeCashFlowPerShareTtm = Some(cash),
              revenueGrowth5Y = Some(revenue),
              epsGrowth5Y = Some(eps),
              dividendYieldAnnual = Some(dividend)
            )
          )
        }
      )

      val filters = List(
        ("all data", StockFilters(), ordered),
        ("exchange", StockFilters(exchange = Some(Exchange.NYSE)), List(low)),
        ("kind", StockFilters(kind = Some(SecurityKind.ETF)), List(low)),
        ("country", StockFilters(country = Some("US")), List(high, mid)),
        ("minimum market cap", StockFilters(minMarketCap = Some(3100000000000L)), List(high, mid)),
        ("maximum market cap", StockFilters(maxMarketCap = Some(3100000000000L)), List(mid, low, missing)),
        ("market cap range", StockFilters(minMarketCap = Some(3000000000000L), maxMarketCap = Some(3200000000000L)), List(mid)),
        ("minimum price", StockFilters(minPrice = Some(BigDecimal(250))), List(high, mid)),
        ("maximum price", StockFilters(maxPrice = Some(BigDecimal(250))), List(mid, low)),
        ("price range", StockFilters(minPrice = Some(BigDecimal(200)), maxPrice = Some(BigDecimal(300))), List(mid)),
        ("minimum change", StockFilters(minChange = Some(BigDecimal(20)), period = Some(TimePeriod.OneYear)), List(high)),
        ("maximum change", StockFilters(maxChange = Some(BigDecimal(10)), period = Some(TimePeriod.ThreeMonth)), List(mid, low)),
        (
          "change range",
          StockFilters(minChange = Some(BigDecimal(5)), maxChange = Some(BigDecimal(10)), period = Some(TimePeriod.OneMonth)),
          List(mid)
        ),
        ("change without period", StockFilters(minChange = Some(BigDecimal(20))), ordered),
        ("minimum overall score", StockFilters(minOverallScore = Some(BigDecimal(60))), List(high, mid)),
        ("minimum CAGR score", StockFilters(minCagrScore = Some(BigDecimal(60))), List(high, mid)),
        ("minimum volatility score", StockFilters(minVolatilityScore = Some(BigDecimal(60))), List(high, mid)),
        ("minimum consistency score", StockFilters(minConsistencyScore = Some(BigDecimal(60))), List(high, mid)),
        ("maximum drawdown", StockFilters(maxDrawdown = Some(BigDecimal(20))), List(high, mid)),
        ("minimum PE", StockFilters(minPE = Some(BigDecimal(25))), List(high, mid)),
        ("maximum PE", StockFilters(maxPE = Some(BigDecimal(25))), List(mid, low)),
        ("PE range", StockFilters(minPE = Some(BigDecimal(15)), maxPE = Some(BigDecimal(35))), List(mid)),
        ("minimum ROE", StockFilters(minROE = Some(BigDecimal(30))), List(high, mid)),
        ("maximum debt to equity", StockFilters(maxDebtToEquity = Some(BigDecimal(1))), List(high, mid)),
        ("minimum profit margin", StockFilters(minProfitMargin = Some(BigDecimal(20))), List(high, mid)),
        ("maximum profit margin", StockFilters(maxProfitMargin = Some(BigDecimal(20))), List(mid, low)),
        ("minimum free cash flow", StockFilters(minFreeCashFlow = Some(BigDecimal(0))), List(high, mid)),
        ("minimum revenue growth", StockFilters(minRevenueGrowth5Y = Some(BigDecimal(12))), List(high, mid)),
        ("minimum EPS growth", StockFilters(minEpsGrowth5Y = Some(BigDecimal(18))), List(high, mid)),
        ("minimum dividend yield", StockFilters(minDividendYield = Some(BigDecimal(2))), List(high, mid)),
        ("maximum dividend yield", StockFilters(maxDividendYield = Some(BigDecimal(2))), List(mid, low)),
        ("unmatched criteria", StockFilters(country = Some("FR")), Nil),
        (
          "combined financial metrics",
          StockFilters(maxPE = Some(BigDecimal(40)), minROE = Some(BigDecimal(30)), maxDebtToEquity = Some(BigDecimal(1))),
          List(mid)
        ),
        (
          "long term investment criteria",
          StockFilters(
            minPE = Some(BigDecimal(15)),
            maxPE = Some(BigDecimal(35)),
            minROE = Some(BigDecimal(15)),
            maxDebtToEquity = Some(BigDecimal("1.5")),
            minProfitMargin = Some(BigDecimal(10)),
            minFreeCashFlow = Some(BigDecimal(0)),
            minRevenueGrowth5Y = Some(BigDecimal(8)),
            minEpsGrowth5Y = Some(BigDecimal(10))
          ),
          List(mid)
        ),
        (
          "all four collections",
          StockFilters(
            exchange = Some(Exchange.NASDAQ),
            country = Some("US"),
            minMarketCap = Some(3000000000000L),
            minOverallScore = Some(BigDecimal(50)),
            maxPE = Some(BigDecimal(35)),
            minROE = Some(BigDecimal(30))
          ),
          List(mid)
        )
      )

      filters.foreach { case (label, filter, expected) =>
        s"filter by $label" in withEmbeddedMongoDatabase(seed) { db =>
          for
            repo   <- StockRepository.make[IO](db)
            result <- repo.findAll(filter, None)
          yield result.map(_.security.ticker) mustBe expected
        }
      }

      List(None, Some(1), Some(10)).foreach { limit =>
        s"apply limit $limit after filtering and sorting" in withEmbeddedMongoDatabase(seed) { db =>
          for
            repo   <- StockRepository.make[IO](db)
            result <- repo.findAll(StockFilters(country = Some("US")), limit)
          yield result.map(_.security.ticker) mustBe List(high, mid).take(limit.getOrElse(2))
        }
      }

      StockSortField.values.toList.foreach { sort =>
        s"sort by $sort without a limit" in {
          val different = AAPLPriceAnalytics.copy(
            ticker = low,
            scores = AAPLPriceAnalytics.scores.copy(overallScore = 100, cagrScore = 100, volatilityScore = 100)
          )
          val changed = seed.updated(
            PriceAnalyticsRepository.CollectionName,
            seed(PriceAnalyticsRepository.CollectionName).filterNot(_.getString("_id").contains(low.value)) :+ toDoc(different)
          )
          withEmbeddedMongoDatabase(changed) { db =>
            for
              repo   <- StockRepository.make[IO](db)
              result <- repo.findAll(StockFilters(sortBy = Some(sort)), None)
            yield result.map(_.security.ticker) mustBe (if sort == StockSortField.MarketCap then ordered else List(low, high, mid, missing))
          }
        }
      }
    }
  }
}
