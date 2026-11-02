package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.domain.{CompanyProfileFilter, ListingStatus, Sector, Ticker}
import stockschecker.fixtures.{ts, AAPL, AAPLCompanyProfile, AAPLSecurity, MSFT, MSFTCompanyProfile, MSFTSecurity}

import java.time.LocalDate
import scala.concurrent.duration.*

class CompanyProfileRepositorySpec extends RepositorySpec {

  override def port: Int = 12147

  "A CompanyProfileRepository" when {
    "save" should {
      "save company profile with its canonical sector in the repository" in
        withEmbeddedMongoDatabase { db =>
          val profile = AAPLCompanyProfile.copy(sector = Some(Sector.Technology))
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(profile)
            res  <- readProfile(db, AAPL)
          yield res mustBe Some(profile)
        }

      "be able to save and update same company profile in the repository" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile.copy(marketCap = 1L))
            res  <- readProfile(db, AAPL)
          yield res.map(_.marketCap) mustBe Some(1L)
        }
    }

    "setActive" should {
      "ignore older requests and accept an equal-time retry within the same millisecond" in
        withActiveStocks(List(AAPL)) { db =>
          val newer = ts.plusNanos(2)
          for
            repo   <- CompanyProfileRepository.make[IO](db)
            _      <- repo.setActive(AAPL, false, ts)
            _      <- repo.setActive(AAPL, true, newer)
            older  <- repo.setActive(AAPL, false, ts.plusNanos(1))
            replay <- repo.setActive(AAPL, true, newer)
            active <- repo.find(AAPL)
          yield
            older mustBe false
            replay mustBe true
            active mustBe defined
        }

      "keep a newer deactivation after a provider refresh and older activation" in
        withActiveStocks(List(AAPL)) { db =>
          for
            repo     <- CompanyProfileRepository.make[IO](db)
            _        <- repo.setActive(AAPL, false, ts.plusNanos(2))
            _        <- repo.save(AAPLCompanyProfile)
            older    <- repo.setActive(AAPL, true, ts.plusNanos(1))
            inactive <- repo.find(AAPL)
          yield
            older mustBe false
            inactive mustBe None
        }

      "clear refresh timestamps on deactivation so reactivated profiles are immediately due" in
        withActiveStocks(List(AAPL)) { db =>
          for
            repo          <- CompanyProfileRepository.make[IO](db)
            _             <- repo.updatePriceAnalyticsLastUpdated(List(AAPL))
            _             <- repo.updateFinancialMetricsLastUpdated(List(AAPL))
            before        <- readProfile(db, AAPL)
            accepted      <- repo.setActive(AAPL, false, ts)
            inactive      <- readProfile(db, AAPL)
            _             <- repo.setActive(AAPL, true, ts.plusSeconds(1))
            pricesDue     <- repo.findTickersBy(CompanyProfileFilter.PriceAnalyticsNotUpdatedFor(3.days), None)
            financialsDue <- repo.findTickersBy(CompanyProfileFilter.FinancialMetricsNotUpdatedFor(3.days), None)
            _             <- repo.updatePriceAnalyticsLastUpdated(List(AAPL))
            _             <- repo.updateFinancialMetricsLastUpdated(List(AAPL))
            stale         <- repo.setActive(AAPL, false, ts)
            active        <- repo.find(AAPL)
          yield
            before.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe defined
            before.flatMap(_.financialMetricsLastUpdatedAt) mustBe defined
            accepted mustBe true
            inactive.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe None
            inactive.flatMap(_.financialMetricsLastUpdatedAt) mustBe None
            pricesDue mustBe List(AAPL)
            financialsDue mustBe List(AAPL)
            stale mustBe false
            active.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe defined
            active.flatMap(_.financialMetricsLastUpdatedAt) mustBe defined
        }

      "accept missing-profile deactivation so orphaned caches can still be deleted" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- CompanyProfileRepository.make[IO](db)
            accepted <- repo.setActive(AAPL, false, ts)
            profile  <- readProfile(db, AAPL)
          yield
            accepted mustBe true
            profile mustBe None
        }
    }

    "findAll" should {
      "return all company profiles sorted by market cap descending" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findAll(None)
          yield res mustBe List(AAPLCompanyProfile, MSFTCompanyProfile)
        }

      "return limited number of company profiles when limit is specified" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findAll(Some(1))
          yield
            res.size mustBe 1
            res.head mustBe AAPLCompanyProfile
        }

      "return empty list when no company profiles exist" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            res  <- repo.findAll(None)
          yield res mustBe List.empty
        }

      "return all profiles when limit is greater than available profiles" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findAll(Some(10))
          yield res.size mustBe 2
        }
    }

    "findTickersBy" should {
      "require an active listed security when selecting active profiles" in {
        val delisted = Ticker("DELISTED")
        val missing  = Ticker("MISSING")
        val seed     = Map(
          CompanyProfileRepository.CollectionName -> List(
            toDoc(AAPLCompanyProfile),
            toDoc(MSFTCompanyProfile),
            toDoc(AAPLCompanyProfile.copy(ticker = delisted)),
            toDoc(AAPLCompanyProfile.copy(ticker = missing))
          ),
          SecurityRepository.CollectionName -> List(
            toDoc(AAPLSecurity),
            toDoc(MSFTSecurity.copy(isActive = false)),
            toDoc(AAPLSecurity.copy(ticker = delisted, listingStatus = ListingStatus.Delisted))
          )
        )
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo                <- CompanyProfileRepository.make[IO](db)
            res                 <- repo.findTickersBy(CompanyProfileFilter.IsActive(true), None)
            withoutActiveFilter <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), None)
            inactive            <- repo.find(MSFT)
          yield
            res mustBe List(AAPL)
            withoutActiveFilter mustBe List(AAPL)
            inactive mustBe None
        }
      }

      "apply nested active-security eligibility before the market-cap limit" in {
        val smaller = Ticker("SMALLER")
        val seed    = Map(
          CompanyProfileRepository.CollectionName -> List(
            toDoc(AAPLCompanyProfile),
            toDoc(MSFTCompanyProfile),
            toDoc(AAPLCompanyProfile.copy(ticker = smaller, marketCap = 1000000000L))
          ),
          SecurityRepository.CollectionName -> List(
            toDoc(AAPLSecurity.copy(isActive = false)),
            toDoc(MSFTSecurity),
            toDoc(AAPLSecurity.copy(ticker = smaller))
          )
        )
        val filter = CompanyProfileFilter.Composite(
          NonEmptyList.of(
            CompanyProfileFilter.CountryIs("US"),
            CompanyProfileFilter.Composite(
              NonEmptyList.of(CompanyProfileFilter.IsActive(true), CompanyProfileFilter.PriceAnalyticsNotUpdatedFor(3.days))
            )
          )
        )
        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo    <- CompanyProfileRepository.make[IO](db)
            limited <- repo.findTickersBy(filter, Some(1))
            all     <- repo.findTickersBy(filter, None)
          yield
            limited mustBe List(MSFT)
            all mustBe List(MSFT, smaller)
        }
      }

      "return tickers filtered by MarketCapAbove" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.MarketCapAbove(3200000000000L), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by MarketCapBelow" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.MarketCapBelow(3200000000000L), None)
          yield res mustBe List(MSFT)
        }

      "return tickers filtered by CountryIs" in
        withActiveStocks(List(AAPL, MSFT, Ticker("HSBC"))) { db =>
          val ukProfile = AAPLCompanyProfile.copy(ticker = Ticker("HSBC"), country = "GB")
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(ukProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("GB"), None)
          yield res mustBe List(Ticker("HSBC"))
        }

      "return tickers filtered by IpoDateAfter" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.IpoDateAfter(LocalDate.parse("1985-01-01")), None)
          yield res mustBe List(MSFT)
        }

      "return tickers filtered by IpoDateBefore" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.IpoDateBefore(LocalDate.parse("1985-01-01")), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by UpdatedWithin" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.UpdatedWithin(50.millis), None)
          yield res mustBe List(MSFT)
        }

      "return tickers filtered by NotUpdatedFor" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.NotUpdatedFor(50.millis), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by TickerMatching with regex pattern" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.TickerMatching(".*SFT"), None)
          yield res mustBe List(MSFT)
        }

      "return empty list when TickerMatching pattern matches nothing" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.TickerMatching("^XYZ.*"), None)
          yield res mustBe List.empty
        }

      "return tickers filtered by Composite filter" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          val composite = CompanyProfileFilter.Composite(
            NonEmptyList.of(
              CompanyProfileFilter.CountryIs("US"),
              CompanyProfileFilter.MarketCapAbove(3200000000000L)
            )
          )
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(composite, None)
          yield res mustBe List(AAPL)
        }

      "respect the limit parameter" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), Some(1))
          yield res.size mustBe 1
        }

      "return results sorted by market cap descending" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), None)
          yield res mustBe List(AAPL, MSFT)
        }

      "return empty list when no profiles match the filter" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("JP"), None)
          yield res mustBe List.empty
        }
    }

    "updatePriceAnalyticsLastUpdated" should {
      "update the pricePerformanceLastUpdated field for single ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo    <- CompanyProfileRepository.make[IO](db)
            _       <- repo.save(AAPLCompanyProfile)
            _       <- repo.updatePriceAnalyticsLastUpdated(List(AAPL))
            updated <- readProfile(db, AAPL)
          yield updated.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe defined
        }

      "not fail when updating non-existent profile" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.updatePriceAnalyticsLastUpdated(List(AAPL))
            res  <- readProfile(db, AAPL)
          yield res mustBe None
        }

      "update the pricePerformanceLastUpdated field for multiple tickers" in
        withEmbeddedMongoDatabase { db =>
          for
            repo        <- CompanyProfileRepository.make[IO](db)
            _           <- repo.save(AAPLCompanyProfile)
            _           <- repo.save(MSFTCompanyProfile)
            _           <- repo.updatePriceAnalyticsLastUpdated(List(AAPL, MSFT))
            updatedAAPL <- readProfile(db, AAPL)
            updatedMSFT <- readProfile(db, MSFT)
          yield
            updatedAAPL.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe defined
            updatedMSFT.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe defined
        }

      "not fail when list is empty" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.updatePriceAnalyticsLastUpdated(List.empty)
            res  <- repo.findAll(None)
          yield res mustBe List.empty
        }

      "only update specified tickers" in
        withEmbeddedMongoDatabase { db =>
          val nonExistent = Ticker("GOOG")
          for
            repo        <- CompanyProfileRepository.make[IO](db)
            _           <- repo.save(AAPLCompanyProfile)
            _           <- repo.save(MSFTCompanyProfile)
            _           <- repo.updatePriceAnalyticsLastUpdated(List(AAPL, nonExistent))
            updatedAAPL <- readProfile(db, AAPL)
            updatedMSFT <- readProfile(db, MSFT)
          yield
            updatedAAPL.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe defined
            updatedMSFT.flatMap(_.priceAnalyticsLastUpdatedAt) mustBe None
        }
    }

    "findTickersBy with PriceAnalyticsNotUpdatedFor filter" should {
      "return tickers where priceAnalyticsLastUpdatedAt is null" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updatePriceAnalyticsLastUpdated(List(MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.PriceAnalyticsNotUpdatedFor(1.hour), None)
          yield res mustBe List(AAPL)
        }

      "return tickers where priceAnalyticsLastUpdatedAt is older than duration" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updatePriceAnalyticsLastUpdated(List(AAPL))
            _    <- IO.sleep(100.millis)
            _    <- repo.updatePriceAnalyticsLastUpdated(List(MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.PriceAnalyticsNotUpdatedFor(50.millis), None)
          yield res mustBe List(AAPL)
        }

      "return empty list when all profiles have been recently updated" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updatePriceAnalyticsLastUpdated(List(AAPL, MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.PriceAnalyticsNotUpdatedFor(1.hour), None)
          yield res mustBe List.empty
        }
    }

    "updateFinancialMetricsLastUpdated" should {
      "update the financialMetricsLastUpdated field for single ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo    <- CompanyProfileRepository.make[IO](db)
            _       <- repo.save(AAPLCompanyProfile)
            _       <- repo.updateFinancialMetricsLastUpdated(List(AAPL))
            updated <- readProfile(db, AAPL)
          yield updated.flatMap(_.financialMetricsLastUpdatedAt) mustBe defined
        }

      "not fail when updating non-existent profile" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.updateFinancialMetricsLastUpdated(List(AAPL))
            res  <- readProfile(db, AAPL)
          yield res mustBe None
        }

      "update the financialMetricsLastUpdated field for multiple tickers" in
        withEmbeddedMongoDatabase { db =>
          for
            repo        <- CompanyProfileRepository.make[IO](db)
            _           <- repo.save(AAPLCompanyProfile)
            _           <- repo.save(MSFTCompanyProfile)
            _           <- repo.updateFinancialMetricsLastUpdated(List(AAPL, MSFT))
            updatedAAPL <- readProfile(db, AAPL)
            updatedMSFT <- readProfile(db, MSFT)
          yield
            updatedAAPL.flatMap(_.financialMetricsLastUpdatedAt) mustBe defined
            updatedMSFT.flatMap(_.financialMetricsLastUpdatedAt) mustBe defined
        }

      "only update specified tickers" in
        withEmbeddedMongoDatabase { db =>
          val nonExistent = Ticker("GOOG")
          for
            repo        <- CompanyProfileRepository.make[IO](db)
            _           <- repo.save(AAPLCompanyProfile)
            _           <- repo.save(MSFTCompanyProfile)
            _           <- repo.updateFinancialMetricsLastUpdated(List(AAPL, nonExistent))
            updatedAAPL <- readProfile(db, AAPL)
            updatedMSFT <- readProfile(db, MSFT)
          yield
            updatedAAPL.flatMap(_.financialMetricsLastUpdatedAt) mustBe defined
            updatedMSFT.flatMap(_.financialMetricsLastUpdatedAt) mustBe None
        }
    }

    "findTickersBy with FinancialMetricsNotUpdatedFor filter" should {
      "return tickers where financialMetricsLastUpdatedAt is null" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updateFinancialMetricsLastUpdated(List(MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.FinancialMetricsNotUpdatedFor(1.hour), None)
          yield res mustBe List(AAPL)
        }

      "return tickers where financialMetricsLastUpdatedAt is older than duration" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updateFinancialMetricsLastUpdated(List(AAPL))
            _    <- IO.sleep(100.millis)
            _    <- repo.updateFinancialMetricsLastUpdated(List(MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.FinancialMetricsNotUpdatedFor(50.millis), None)
          yield res mustBe List(AAPL)
        }
    }
  }
}
