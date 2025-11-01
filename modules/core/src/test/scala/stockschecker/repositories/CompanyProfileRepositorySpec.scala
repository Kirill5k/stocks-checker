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

    "streamTickersBy" should {
      "return tickers filtered by MarketCapAbove" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.streamTickersBy(CompanyProfileFilter.MarketCapAbove(3200000000000L), None).compile.toList
          yield res mustBe List(AAPL)
        }
      }

      "return tickers filtered by MarketCapBelow" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.streamTickersBy(CompanyProfileFilter.MarketCapBelow(3200000000000L), None).compile.toList
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
            res  <- repo.streamTickersBy(CompanyProfileFilter.CountryIs("GB"), None).compile.toList
          yield res mustBe List(Ticker("HSBC"))
        }
      }

      "return tickers filtered by IpoDateAfter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.streamTickersBy(CompanyProfileFilter.IpoDateAfter(LocalDate.parse("1985-01-01")), None).compile.toList
          yield res mustBe List(MSFT)
        }
      }

      "return tickers filtered by IpoDateBefore" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.streamTickersBy(CompanyProfileFilter.IpoDateBefore(LocalDate.parse("1985-01-01")), None).compile.toList
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
            res  <- repo.streamTickersBy(CompanyProfileFilter.UpdatedWithin(50.millis), None).compile.toList
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
            res  <- repo.streamTickersBy(CompanyProfileFilter.NotUpdatedFor(50.millis), None).compile.toList
          yield res mustBe List(AAPL)
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
            res  <- repo.streamTickersBy(composite, None).compile.toList
          yield res mustBe List(AAPL)
        }
      }

      "respect the limit parameter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            _    <- repo.save(MSFTCompanyProfile)
            res  <- repo.streamTickersBy(CompanyProfileFilter.CountryIs("US"), Some(1)).compile.toList
          yield res.size mustBe 1
        }
      }

      "return results sorted by market cap descending" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(MSFTCompanyProfile)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.streamTickersBy(CompanyProfileFilter.CountryIs("US"), None).compile.toList
          yield res mustBe List(AAPL, MSFT)
        }
      }

      "return empty list when no profiles match the filter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CompanyProfileRepository.make[IO](db)
            _    <- repo.save(AAPLCompanyProfile)
            res  <- repo.streamTickersBy(CompanyProfileFilter.CountryIs("JP"), None).compile.toList
          yield res mustBe List.empty
        }
      }
    }
  }
}
