package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.IO
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.fixtures.{AAPL, AAPLCompanyProfile, MSFT, MSFTCompanyProfile}

import java.time.LocalDate
import scala.concurrent.Future
import scala.concurrent.duration.*

class CompanyProfileRepositorySpec extends RepositorySpec {

  override def port: Int = 12147

  "A CompanyProfileRepository" when {
    "save" should {
      "save company profile in the repository" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.find(AAPL)
          yield res mustBe Some(AAPLCompanyProfile)
        }
      }

      "be able to save and update same company profile in the repository" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile.copy(marketCap = 1L))
            res  <- repo.find(AAPL)
          yield res.map(_.marketCap) mustBe Some(1L)
        }
      }
    }

    "findAll" should {
      "return all company profiles sorted by market cap descending" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findAll(None)
          yield res mustBe List(AAPLCompanyProfile, MSFTCompanyProfile)
        }
      }

      "return limited number of company profiles when limit is specified" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findAll(Some(1))
          yield 
            res.size mustBe 1
            res.head mustBe AAPLCompanyProfile
        }
      }

      "return empty list when no company profiles exist" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            res  <- repo.findAll(None)
          yield res mustBe List.empty
        }
      }

      "return all profiles when limit is greater than available profiles" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findAll(Some(10))
          yield res.size mustBe 2
        }
      }
    }

    "findTickersBy" should {
      "return tickers filtered by MarketCapAbove" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.MarketCapAbove(3200000000000L), None)
          yield res mustBe List(AAPL)
        }
      }

      "return tickers filtered by MarketCapBelow" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.MarketCapBelow(3200000000000L), None)
          yield res mustBe List(MSFT)
        }
      }

      "return tickers filtered by CountryIs" in {
        withEmbeddedMongoDatabase { db =>
          val ukProfile = AAPLCompanyProfile.copy(ticker = Ticker("HSBC"), country = "GB")
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(ukProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("GB"), None)
          yield res mustBe List(Ticker("HSBC"))
        }
      }

      "return tickers filtered by IpoDateAfter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.IpoDateAfter(LocalDate.parse("1985-01-01")), None)
          yield res mustBe List(MSFT)
        }
      }

      "return tickers filtered by IpoDateBefore" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.IpoDateBefore(LocalDate.parse("1985-01-01")), None)
          yield res mustBe List(AAPL)
        }
      }

      "return tickers filtered by UpdatedWithin" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.UpdatedWithin(50.millis), None)
          yield res mustBe List(MSFT)
        }
      }

      "return tickers filtered by NotUpdatedFor" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.NotUpdatedFor(50.millis), None)
          yield res mustBe List(AAPL)
        }
      }

      "return tickers filtered by TickerMatching with regex pattern" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.TickerMatching(".*SFT"), None)
          yield res mustBe List(MSFT)
        }
      }

      "return empty list when TickerMatching pattern matches nothing" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.TickerMatching("^XYZ.*"), None)
          yield res mustBe List.empty
        }
      }

      "return tickers filtered by Composite filter" in {
        withEmbeddedMongoDatabase { db =>
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
      }

      "respect the limit parameter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), Some(1))
          yield res.size mustBe 1
        }
      }

      "return results sorted by market cap descending" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("US"), None)
          yield res mustBe List(AAPL, MSFT)
        }
      }

      "return empty list when no profiles match the filter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.findTickersBy(CompanyProfileFilter.CountryIs("JP"), None)
          yield res mustBe List.empty
        }
      }
    }

    "updatePricePerformanceLastUpdated" should {
      "update the pricePerformanceLastUpdated field for single ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo    <- CompanyProfileRepository.make[IO](db)
            _       <- repo.save(AAPLCompanyProfile)
            _       <- repo.updatePricePerformanceLastUpdated(List(AAPL))
            updated <- repo.find(AAPL)
          yield updated.flatMap(_.pricePerformanceLastUpdatedAt) mustBe defined
        }
      }

      "not fail when updating non-existent profile" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.updatePricePerformanceLastUpdated(List(AAPL))
            res  <- repo.find(AAPL)
          yield res mustBe None
        }
      }

      "update the pricePerformanceLastUpdated field for multiple tickers" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo        <- CompanyProfileRepository.make[IO](db)
            _           <- repo.save(AAPLCompanyProfile)
            _           <- repo.save(MSFTCompanyProfile)
            _           <- repo.updatePricePerformanceLastUpdated(List(AAPL, MSFT))
            updatedAAPL <- repo.find(AAPL)
            updatedMSFT <- repo.find(MSFT)
          yield
            updatedAAPL.flatMap(_.pricePerformanceLastUpdatedAt) mustBe defined
            updatedMSFT.flatMap(_.pricePerformanceLastUpdatedAt) mustBe defined
        }
      }

      "not fail when list is empty" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.updatePricePerformanceLastUpdated(List.empty)
            res  <- repo.findAll(None)
          yield res mustBe List.empty
        }
      }

      "only update specified tickers" in {
        withEmbeddedMongoDatabase { db =>
          val nonExistent = Ticker("GOOG")
          for
            repo        <- CompanyProfileRepository.make[IO](db)
            _           <- repo.save(AAPLCompanyProfile)
            _           <- repo.save(MSFTCompanyProfile)
            _           <- repo.updatePricePerformanceLastUpdated(List(AAPL, nonExistent))
            updatedAAPL <- repo.find(AAPL)
            updatedMSFT <- repo.find(MSFT)
          yield
            updatedAAPL.flatMap(_.pricePerformanceLastUpdatedAt) mustBe defined
            updatedMSFT.flatMap(_.pricePerformanceLastUpdatedAt) mustBe None
        }
      }
    }

    "findTickersBy with PricePerformanceNotUpdatedFor filter" should {
      "return tickers where pricePerformanceLastUpdatedAt is null" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updatePricePerformanceLastUpdated(List(MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.PricePerformanceNotUpdatedFor(1.hour), None)
          yield res mustBe List(AAPL)
        }
      }

      "return tickers where pricePerformanceLastUpdatedAt is older than duration" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updatePricePerformanceLastUpdated(List(AAPL))
            _    <- IO.sleep(100.millis)
            _    <- repo.updatePricePerformanceLastUpdated(List(MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.PricePerformanceNotUpdatedFor(50.millis), None)
          yield res mustBe List(AAPL)
        }
      }

      "return empty list when all profiles have been recently updated" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.updatePricePerformanceLastUpdated(List(AAPL, MSFT))
            res  <- repo.findTickersBy(CompanyProfileFilter.PricePerformanceNotUpdatedFor(1.hour), None)
          yield res mustBe List.empty
        }
      }
    }
  }
}
