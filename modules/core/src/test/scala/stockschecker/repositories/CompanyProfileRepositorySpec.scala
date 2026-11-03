package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import stockschecker.domain.{CompanyProfileFilter, ListingStatus, Sector, Ticker}
import stockschecker.fixtures.{AAPL, AAPLCompanyProfile, AAPLSecurity, MSFT, MSFTCompanyProfile, MSFTSecurity}

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

    "delete" should {
      "remove only the requested profile and allow repeated cleanup" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo     <- CompanyProfileRepository.make[IO](db)
            _        <- repo.delete(AAPL)
            _        <- repo.delete(AAPL)
            deleted  <- readProfile(db, AAPL)
            retained <- readProfile(db, MSFT)
          yield
            deleted mustBe None
            retained mustBe defined
        }
    }

    "findAll" should {
      "return all company profiles sorted by market cap descending" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findAll(None)
          yield res mustBe List(AAPLCompanyProfile, MSFTCompanyProfile)
        }

      "return limited number of company profiles when limit is specified" in
        withProfiles(List(AAPL, MSFT)) { db =>
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
        withProfiles(List(AAPL, MSFT)) { db =>
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
            inactiveSelection   <- repo.findTickersBy(CompanyProfileFilter.IsActive(false), None)
            withoutActiveFilter <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), None)
            inactive            <- repo.find(MSFT)
          yield
            res mustBe List(AAPL)
            inactiveSelection mustBe Nil
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
            repo     <- CompanyProfileRepository.make[IO](db)
            limited  <- repo.findTickersBy(filter, Some(1))
            all      <- repo.findTickersBy(filter, None)
            inactive <- repo.findTickersBy(
              CompanyProfileFilter.Composite(NonEmptyList.of(filter, CompanyProfileFilter.IsActive(false))),
              None
            )
          yield
            limited mustBe List(MSFT)
            all mustBe List(MSFT, smaller)
            inactive mustBe Nil
        }
      }

      "return tickers filtered by MarketCapAbove" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.MarketCapAbove(3200000000000L), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by MarketCapBelow" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.MarketCapBelow(3200000000000L), None)
          yield res mustBe List(MSFT)
        }

      "return tickers filtered by CountryIs" in
        withProfiles(List(AAPL, MSFT, Ticker("HSBC"))) { db =>
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
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.IpoDateAfter(LocalDate.parse("1985-01-01")), None)
          yield res mustBe List(MSFT)
        }

      "return tickers filtered by IpoDateBefore" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.IpoDateBefore(LocalDate.parse("1985-01-01")), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by UpdatedWithin" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.UpdatedWithin(50.millis), None)
          yield res mustBe List(MSFT)
        }

      "return tickers filtered by NotUpdatedFor" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.NotUpdatedFor(50.millis), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by TickerMatching with regex pattern" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.TickerMatching(".*SFT"), None)
          yield res mustBe List(MSFT)
        }

      "return empty list when TickerMatching pattern matches nothing" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.TickerMatching("^XYZ.*"), None)
          yield res mustBe List.empty
        }

      "return tickers filtered by Composite filter" in
        withProfiles(List(AAPL, MSFT)) { db =>
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
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), Some(1))
          yield res.size mustBe 1
        }

      "return results sorted by market cap descending" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), None)
          yield res mustBe List(AAPL, MSFT)
        }

      "return empty list when no profiles match the filter" in
        withProfiles(List(AAPL, MSFT)) { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("JP"), None)
          yield res mustBe List.empty
        }
    }

    val timestamps: List[
      (
          String,
          CompanyProfileRepository[IO] => List[Ticker] => IO[Unit],
          stockschecker.domain.CompanyProfile => Option[java.time.Instant],
          FiniteDuration => CompanyProfileFilter
      )
    ] = List(
      (
        "price analytics",
        repo => repo.updatePriceAnalyticsLastUpdated,
        _.priceAnalyticsLastUpdatedAt,
        CompanyProfileFilter.PriceAnalyticsNotUpdatedFor(_)
      ),
      (
        "financial metrics",
        repo => repo.updateFinancialMetricsLastUpdated,
        _.financialMetricsLastUpdatedAt,
        CompanyProfileFilter.FinancialMetricsNotUpdatedFor(_)
      )
    )
    timestamps.foreach { case (name, update, timestamp, stale) =>
      s"$name timestamps" should {
        "update only existing requested tickers and tolerate empty requests" in
          withProfiles(List(AAPL, MSFT)) { db =>
            for
              repo      <- CompanyProfileRepository.make[IO](db)
              _         <- update(repo)(List.empty)
              _         <- update(repo)(List(AAPL, Ticker("MISSING")))
              updated   <- readProfile(db, AAPL)
              unchanged <- readProfile(db, MSFT)
              missing   <- readProfile(db, Ticker("MISSING"))
              due       <- repo.findTickersBy(stale(1.hour), None)
            yield
              updated.flatMap(timestamp) mustBe defined
              unchanged.flatMap(timestamp) mustBe None
              missing mustBe None
              due mustBe List(MSFT)
          }

        "refresh multiple tickers together" in
          withProfiles(List(AAPL, MSFT)) { db =>
            for
              repo <- CompanyProfileRepository.make[IO](db)
              _    <- update(repo)(List(AAPL, MSFT))
              due  <- repo.findTickersBy(stale(1.hour), None)
            yield due mustBe Nil
          }

        "select old timestamps while excluding recent timestamps" in
          withProfiles(List(AAPL, MSFT)) { db =>
            for
              repo <- CompanyProfileRepository.make[IO](db)
              _    <- update(repo)(List(AAPL))
              _    <- IO.sleep(100.millis)
              _    <- update(repo)(List(MSFT))
              due  <- repo.findTickersBy(stale(50.millis), None)
            yield due mustBe List(AAPL)
          }
      }
    }
  }
}
