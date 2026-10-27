package stockschecker.repositories

import stockschecker.domain.ListingStatus
import cats.data.NonEmptyList
import cats.effect.IO
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.domain.{Exchange, Security, SecurityFilter, SecurityKind, Ticker}
import stockschecker.fixtures.*

import scala.concurrent.Future
import scala.concurrent.duration.*

class SecurityRepositorySpec extends RepositorySpec {

  override def port: Int = 12149

  "A SecurityRepository" when {
    "save" should {
      "store security in database" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            all  <- repo.streamAll.compile.toList
          yield all mustBe List(AAPLSecurity)
        }

      "store multiple securities in database" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            securities = List(AAPLSecurity, MSFTSecurity)
            _   <- repo.save(securities)
            all <- repo.streamAll.compile.toList
          yield all mustBe securities
        }
    }

    "reconcile" should {
      "preserve manual exclusions when the provider still lists the security" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- SecurityRepository.make(db)
            _        <- repo.save(List(AAPLSecurity, MSFTSecurity))
            _        <- repo.deactivate(AAPL)
            _        <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
            excluded <- repo.find(AAPL)
            listed   <- repo.find(MSFT)
          yield
            excluded.map(_.isActive) mustBe Some(false)
            excluded.flatMap(_.exclusionReason) mustBe Some("user-excluded")
            excluded.map(_.listingStatus) mustBe Some(ListingStatus.Listed)
            listed.map(_.isActive) mustBe Some(true)
        }

      "keep delisted securities inactive after restore until a successful listing reconciliation" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- SecurityRepository.make(db)
            _        <- repo.save(List(AAPLSecurity, MSFTSecurity))
            _        <- repo.deactivate(AAPL)
            _        <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
            delisted <- repo.find(AAPL)
            _        <- repo.activate(AAPL)
            restored <- repo.find(AAPL)
            _        <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
            relisted <- repo.find(AAPL)
          yield
            delisted.map(_.listingStatus) mustBe Some(ListingStatus.Delisted)
            restored.map(_.isActive) mustBe Some(false)
            restored.map(_.listingStatus) mustBe Some(ListingStatus.Delisted)
            restored.flatMap(_.exclusionReason) mustBe None
            relisted.map(_.isActive) mustBe Some(true)
            relisted.map(_.listingStatus) mustBe Some(ListingStatus.Listed)
        }

      "leave existing listing state unchanged for an empty reconciliation" in
        withEmbeddedMongoDatabase { db =>
          for
            repo   <- SecurityRepository.make(db)
            _      <- repo.save(AAPLSecurity)
            _      <- repo.reconcile(Exchange.NASDAQ, Nil)
            result <- repo.find(AAPL)
          yield result mustBe Some(AAPLSecurity)
        }
    }

    "findByTicker" should {
      "find security by ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            s    <- repo.find(AAPL)
          yield s mustBe Some(AAPLSecurity)
        }

      "return None when security not found" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            s    <- repo.find(AAPL)
          yield s mustBe None
        }
    }

    "findByExchange" should {
      "find securities by exchange" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            s    <- repo.findByExchange(Exchange.NASDAQ)
          yield s must contain theSameElementsAs List(AAPLSecurity, MSFTSecurity)
        }
    }

    "findExcluded" should {
      "return only securities with an exclusion reason regardless of listing status" in {
        val persisted = AAPLSecurity.copy(isActive = false, exclusionReason = Some("user-excluded"))
        val manual    = MSFTSecurity.copy(isActive = false, exclusionReason = Some("user-excluded"))
        val excluded  = AAPLSecurity.copy(ticker = Ticker("EXCLUDED"))
        val delisted  = AAPLSecurity.copy(ticker = Ticker("DELISTED"))
        val restored  = AAPLSecurity.copy(ticker = Ticker("RESTORED"))
        val active    = AAPLSecurity.copy(ticker = Ticker("ACTIVE"))
        val seed      = Map(SecurityRepository.CollectionName -> List(toDoc(persisted)))

        withEmbeddedMongoDatabase(seed) { db =>
          for
            repo   <- SecurityRepository.make(db)
            _      <- repo.save(List(MSFTSecurity, excluded, delisted, restored, active))
            _      <- repo.deactivate(MSFT)
            _      <- repo.deactivate(excluded.ticker)
            _      <- repo.deactivate(restored.ticker)
            _      <- repo.activate(restored.ticker)
            _      <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity, restored, active))
            result <- repo.findExcluded
            _      <- repo.activate(AAPL)
            _      <- repo.activate(MSFT)
            _      <- repo.activate(excluded.ticker)
            empty  <- repo.findExcluded
          yield
            result must contain theSameElementsAs List(
              persisted,
              manual,
              excluded.copy(isActive = false, listingStatus = ListingStatus.Delisted, exclusionReason = Some("user-excluded"))
            )
            empty mustBe List.empty[Security]
        }
      }
    }

    "getAllTickers" should {
      "return tickers of all securities stored in db" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            s    <- repo.getAllTickers
          yield s must contain theSameElementsAs List(AAPL, MSFT)
        }
    }

    "findTickersBy" should {
      "return tickers filtered by ExchangeIs" in
        withEmbeddedMongoDatabase { db =>
          val nyseSec = AAPLSecurity.copy(ticker = Ticker("IBM"), exchange = Exchange.NYSE)
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            _    <- repo.save(nyseSec)
            res  <- repo.findTickersBy(SecurityFilter.ExchangeIs(Exchange.NASDAQ), None)
          yield res must contain theSameElementsAs List(AAPL, MSFT)
        }

      "return tickers filtered by KindIs" in
        withEmbeddedMongoDatabase { db =>
          val etfSec = AAPLSecurity.copy(ticker = Ticker("SPY"), kind = SecurityKind.ETF)
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(etfSec)
            res  <- repo.findTickersBy(SecurityFilter.KindIs(SecurityKind.ETF), None)
          yield res mustBe List(Ticker("SPY"))
        }

      "return tickers filtered by IsActive" in
        withEmbeddedMongoDatabase { db =>
          val inactiveTicker = Ticker("INACTIVE")
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(AAPLSecurity.copy(ticker = inactiveTicker))
            _    <- repo.deactivate(inactiveTicker)
            res  <- repo.findTickersBy(SecurityFilter.IsActive(true), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by UpdatedWithin" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTSecurity)
            res  <- repo.findTickersBy(SecurityFilter.UpdatedWithin(50.millis), None)
          yield res mustBe List(MSFT)
        }

      "return tickers filtered by NotUpdatedFor" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTSecurity)
            res  <- repo.findTickersBy(SecurityFilter.NotUpdatedFor(50.millis), None)
          yield res mustBe List(AAPL)
        }

      "return tickers filtered by Composite filter" in
        withEmbeddedMongoDatabase { db =>
          val composite = SecurityFilter.Composite(
            NonEmptyList.of(
              SecurityFilter.ExchangeIs(Exchange.NASDAQ),
              SecurityFilter.KindIs(SecurityKind.Stock)
            )
          )
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            res  <- repo.findTickersBy(composite, None)
          yield res must contain theSameElementsAs List(AAPL, MSFT)
        }

      "respect the limit parameter" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            res  <- repo.findTickersBy(SecurityFilter.ExchangeIs(Exchange.NASDAQ), Some(1))
          yield res.size mustBe 1
        }

      "return empty list when no securities match the filter" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            res  <- repo.findTickersBy(SecurityFilter.ExchangeIs(Exchange.NYSE), None)
          yield res mustBe List.empty
        }
    }

    "updateCompanyProfileLastUpdated" should {
      "update the companyProfileLastUpdated field for single ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo    <- SecurityRepository.make(db)
            _       <- repo.save(AAPLSecurity)
            _       <- repo.updateCompanyProfileLastUpdated(List(AAPL))
            updated <- repo.find(AAPL)
          yield updated.flatMap(_.companyProfileLastUpdatedAt) mustBe defined
        }

      "do nothing if security does not exist" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.updateCompanyProfileLastUpdated(List(AAPL))
            res  <- repo.find(AAPL)
          yield res mustBe None
        }

      "update the companyProfileLastUpdated field for multiple tickers" in
        withEmbeddedMongoDatabase { db =>
          for
            repo        <- SecurityRepository.make(db)
            _           <- repo.save(AAPLSecurity)
            _           <- repo.save(MSFTSecurity)
            _           <- repo.updateCompanyProfileLastUpdated(List(AAPL, MSFT))
            updatedAAPL <- repo.find(AAPL)
            updatedMSFT <- repo.find(MSFT)
          yield {
            updatedAAPL.flatMap(_.companyProfileLastUpdatedAt) mustBe defined
            updatedMSFT.flatMap(_.companyProfileLastUpdatedAt) mustBe defined
          }
        }

      "handle empty list gracefully" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.updateCompanyProfileLastUpdated(List.empty)
            res  <- repo.find(AAPL)
          yield res mustBe Some(AAPLSecurity)
        }

      "update only existing securities when given mixed list" in
        withEmbeddedMongoDatabase { db =>
          val nonExistent = Ticker("NONEXIST")
          for
            repo         <- SecurityRepository.make(db)
            _            <- repo.save(AAPLSecurity)
            _            <- repo.updateCompanyProfileLastUpdated(List(AAPL, nonExistent))
            updatedAAPL  <- repo.find(AAPL)
            updatedOther <- repo.find(nonExistent)
          yield {
            updatedAAPL.flatMap(_.companyProfileLastUpdatedAt) mustBe defined
            updatedOther mustBe None
          }
        }
    }
  }
}
