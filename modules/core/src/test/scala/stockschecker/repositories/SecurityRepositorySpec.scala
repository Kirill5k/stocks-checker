package stockschecker.repositories

import stockschecker.domain.ListingStatus
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.traverse.*
import fs2.Stream
import kirill5k.common.cats.Clock
import mongo4cats.operations.Filter
import org.mockito.Mockito.{mock, times, verify, verifyNoInteractions, when}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{Exchange, SecurityFilter, SecurityKind, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.services.{CompanyProfileService, SecurityService}

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
            all  <- readSecurity(db, AAPL)
          yield all mustBe Some(AAPLSecurity)
        }

      "store multiple securities in database" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            securities = List(AAPLSecurity, MSFTSecurity)
            _   <- repo.save(securities)
            all <- List(AAPL, MSFT).traverse(readSecurity(db, _))
          yield all.flatten mustBe securities
        }
    }

    "setActive" should {
      "resume profile deactivation and cache deletion after dispatch fails following the security write" in
        withActiveStocks(
          Map(
            SecurityRepository.CollectionName         -> List(toDoc(AAPLSecurity)),
            PriceAnalyticsRepository.CollectionName   -> List(toDoc(AAPLPriceAnalytics)),
            FinancialMetricsRepository.CollectionName -> List(toDoc(AAPLFinancialMetrics))
          )
        ) { db =>
          given Clock[IO]  = Clock.make[IO]
          given Logger[IO] = Slf4jLogger.getLogger[IO]
          val dispatcher   = mock(classOf[ActionDispatcher[IO]])
          val client       = mock(classOf[MarketDataClient[IO]])
          val failure      = new com.mongodb.MongoException("profile dispatch unavailable")
          for
            securities     <- SecurityRepository.make[IO](db)
            profiles       <- CompanyProfileRepository.make[IO](db)
            prices         <- PriceAnalyticsRepository.make[IO](db)
            metrics        <- FinancialMetricsRepository.make[IO](db)
            profileService <- CompanyProfileService.make(profiles, client, dispatcher)
            service        <- SecurityService.make(securities, client, dispatcher)
            rawProfiles    <- db.getCollection(CompanyProfileRepository.CollectionName)
            _ = when(dispatcher.dispatch(Action.SetCompanyProfileActive(AAPL, false, ts)))
              .thenReturn(IO.raiseError(failure), profileService.setActive(AAPL, false, ts))
            _ = when(dispatcher.dispatch(Action.DeletePriceAnalytics(AAPL))).thenReturn(prices.delete(AAPL))
            _ = when(dispatcher.dispatch(Action.DeleteFinancialMetrics(AAPL))).thenReturn(metrics.delete(AAPL))
            first       <- service.setActive(AAPL, false, ts).attempt
            excluded    <- readSecurity(db, AAPL)
            beforeRetry <- rawProfiles.find(Filter.idEq(AAPL.value)).first
            _           <- service.setActive(AAPL, false, ts)
            afterRetry  <- rawProfiles.find(Filter.idEq(AAPL.value)).first
            price       <- prices.find(AAPL)
            financials  <- metrics.find(AAPL)
          yield
            first mustBe Left(failure)
            excluded.map(_.isActive) mustBe Some(false)
            beforeRetry.flatMap(_.getBoolean("isActive")) mustBe Some(true)
            afterRetry.flatMap(_.getBoolean("isActive")) mustBe Some(false)
            price mustBe None
            financials mustBe None
            verify(dispatcher, times(2)).dispatch(Action.SetCompanyProfileActive(AAPL, false, ts))
            verifyNoInteractions(client)
            succeed
        }

      "ignore older requests and accept retries of the current activation" in
        withEmbeddedMongoDatabase { db =>
          val newer = ts.plusNanos(2)
          for
            repo      <- SecurityRepository.make(db)
            _         <- repo.save(AAPLSecurity)
            _         <- repo.setActive(AAPL, false, ts)
            _         <- repo.setActive(AAPL, true, newer)
            older     <- repo.setActive(AAPL, false, ts.plusNanos(1))
            duplicate <- repo.setActive(AAPL, true, newer)
            result    <- readSecurity(db, AAPL)
          yield
            older mustBe false
            duplicate mustBe true
            result.map(_.isActive) mustBe Some(true)
            result.flatMap(_.exclusionReason) mustBe None
        }

      "ignore an older activation after a newer deactivation, even after a provider refresh" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- SecurityRepository.make(db)
            _        <- repo.save(AAPLSecurity)
            _        <- repo.setActive(AAPL, false, ts.plusNanos(2))
            _        <- repo.save(AAPLSecurity)
            rejected <- repo.setActive(AAPL, true, ts.plusNanos(1))
            result   <- readSecurity(db, AAPL)
          yield
            rejected mustBe false
            result.map(_.isActive) mustBe Some(false)
            result.flatMap(_.exclusionReason) mustBe Some("user-excluded")
        }

      "ignore a missing security without inserting one" in
        withEmbeddedMongoDatabase { db =>
          for
            repo    <- SecurityRepository.make(db)
            result  <- repo.setActive(AAPL, false, ts)
            missing <- readSecurity(db, AAPL)
          yield
            result mustBe false
            missing mustBe None
        }
    }

    "reconcile" should {
      "retain profile and cached data during a listing gap and restore visibility when the ticker returns" in
        withActiveStocks(
          Map(
            SecurityRepository.CollectionName         -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
            PriceAnalyticsRepository.CollectionName   -> List(toDoc(AAPLPriceAnalytics)),
            FinancialMetricsRepository.CollectionName -> List(toDoc(AAPLFinancialMetrics))
          )
        ) { db =>
          given Logger[IO] = Slf4jLogger.getLogger[IO]
          val client       = mock(classOf[MarketDataClient[IO]])
          val dispatcher   = mock(classOf[ActionDispatcher[IO]])
          when(client.getTradedSecurities(Exchange.NASDAQ)).thenReturn(
            Stream.emit(MSFTSecurity).covary[IO],
            Stream.emits(List(AAPLSecurity, MSFTSecurity)).covary[IO]
          )
          for
            securities <- SecurityRepository.make[IO](db)
            prices     <- PriceAnalyticsRepository.make[IO](db)
            metrics    <- FinancialMetricsRepository.make[IO](db)
            service    <- SecurityService.make(securities, client, dispatcher)
            before     <- readProfile(db, AAPL)
            _          <- service.fetchLatest(NonEmptyList.one(Exchange.NASDAQ))
            hidden     <- securities.findVisible(AAPL)
            retained   <- readProfile(db, AAPL)
            price      <- prices.find(AAPL)
            financials <- metrics.find(AAPL)
            _          <- service.fetchLatest(NonEmptyList.one(Exchange.NASDAQ))
            restored   <- securities.findVisible(AAPL)
          yield
            hidden mustBe None
            retained mustBe before
            price mustBe Some(AAPLPriceAnalytics)
            financials mustBe Some(AAPLFinancialMetrics)
            restored mustBe Some(AAPLSecurity)
            verifyNoInteractions(dispatcher)
            succeed
        }

      "delist missing tickers only in the reconciled exchange" in
        withEmbeddedMongoDatabase { db =>
          val otherExchange = AAPLSecurity.copy(ticker = Ticker("IBM"), exchange = Exchange.NYSE)
          for
            repo      <- SecurityRepository.make(db)
            _         <- repo.save(List(AAPLSecurity, MSFTSecurity, otherExchange))
            _         <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
            missing   <- readSecurity(db, AAPL)
            listed    <- readSecurity(db, MSFT)
            untouched <- readSecurity(db, otherExchange.ticker)
          yield
            missing.map(_.listingStatus) mustBe Some(ListingStatus.Delisted)
            missing.map(_.isActive) mustBe Some(false)
            listed mustBe Some(MSFTSecurity)
            untouched mustBe Some(otherExchange)
        }

      "preserve manual exclusions while delisting and relisting a missing ticker" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- SecurityRepository.make(db)
            _        <- repo.save(List(AAPLSecurity, MSFTSecurity))
            _        <- repo.setActive(AAPL, false, ts)
            _        <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
            missing  <- readSecurity(db, AAPL)
            _        <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
            relisted <- readSecurity(db, AAPL)
          yield
            missing.map(_.listingStatus) mustBe Some(ListingStatus.Delisted)
            missing.flatMap(_.exclusionReason) mustBe Some("user-excluded")
            relisted.map(_.listingStatus) mustBe Some(ListingStatus.Listed)
            relisted.map(_.isActive) mustBe Some(false)
            relisted.flatMap(_.exclusionReason) mustBe Some("user-excluded")
        }

      "preserve manual exclusions when the provider still lists the security" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- SecurityRepository.make(db)
            _        <- repo.save(List(AAPLSecurity, MSFTSecurity))
            _        <- repo.setActive(AAPL, false, ts)
            _        <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
            excluded <- readSecurity(db, AAPL)
            listed   <- readSecurity(db, MSFT)
          yield
            excluded.map(_.isActive) mustBe Some(false)
            excluded.flatMap(_.exclusionReason) mustBe Some("user-excluded")
            excluded.map(_.listingStatus) mustBe Some(ListingStatus.Listed)
            listed.map(_.isActive) mustBe Some(true)
        }

      "reject delisted activation and preserve the exclusion until explicitly activated after relisting" in
        withEmbeddedMongoDatabase { db =>
          for
            repo      <- SecurityRepository.make(db)
            _         <- repo.save(List(AAPLSecurity, MSFTSecurity))
            _         <- repo.setActive(AAPL, false, ts)
            _         <- repo.reconcile(Exchange.NASDAQ, List(MSFTSecurity))
            delisted  <- readSecurity(db, AAPL)
            rejected  <- repo.setActive(AAPL, true, ts.plusSeconds(1)).attempt
            restored  <- readSecurity(db, AAPL)
            _         <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
            relisted  <- readSecurity(db, AAPL)
            _         <- repo.setActive(AAPL, true, ts.plusSeconds(2))
            activated <- readSecurity(db, AAPL)
          yield
            rejected.left.toOption.get mustBe a[AppError.FailedValidation]
            delisted.map(_.listingStatus) mustBe Some(ListingStatus.Delisted)
            restored.map(_.isActive) mustBe Some(false)
            restored.map(_.listingStatus) mustBe Some(ListingStatus.Delisted)
            restored.flatMap(_.exclusionReason) mustBe Some("user-excluded")
            relisted.map(_.isActive) mustBe Some(false)
            relisted.map(_.listingStatus) mustBe Some(ListingStatus.Listed)
            activated.map(_.isActive) mustBe Some(true)
            activated.flatMap(_.exclusionReason) mustBe None
        }

      "leave existing listing state unchanged for an empty reconciliation" in
        withEmbeddedMongoDatabase { db =>
          for
            repo   <- SecurityRepository.make(db)
            _      <- repo.save(AAPLSecurity)
            _      <- repo.reconcile(Exchange.NASDAQ, Nil)
            result <- readSecurity(db, AAPL)
          yield result mustBe Some(AAPLSecurity)
        }
    }

    "findVisible" should {
      "find security by ticker" in
        withActiveStocks(List(AAPL)) { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            s    <- repo.findVisible(AAPL)
          yield s mustBe Some(AAPLSecurity)
        }

      "return None when security not found" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            s    <- repo.findVisible(AAPL)
          yield s mustBe None
        }
    }

    "findByExchange" should {
      "find securities by exchange" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            s    <- repo.findByExchange(Exchange.NASDAQ)
          yield s must contain theSameElementsAs List(AAPLSecurity, MSFTSecurity)
        }
    }

    "getAllTickers" should {
      "return tickers of all securities stored in db" in
        withActiveStocks(List(AAPL, MSFT)) { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            s    <- repo.getAllTickers
          yield s must contain theSameElementsAs List(AAPL, MSFT)
        }
    }

    "findTickersBy" should {
      "exclude inactive and delisted securities before limiting without requiring an active filter" in
        withEmbeddedMongoDatabase { db =>
          val delisted = AAPLSecurity.copy(ticker = Ticker("DELISTED"))
          for
            repo     <- SecurityRepository.make(db)
            _        <- repo.save(List(AAPLSecurity, delisted, MSFTSecurity))
            _        <- repo.setActive(AAPL, false, ts)
            _        <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
            result   <- repo.findTickersBy(SecurityFilter.ExchangeIs(Exchange.NASDAQ), Some(1))
            inactive <- repo.findTickersBy(SecurityFilter.IsActive(false), None)
          yield
            result mustBe List(MSFT)
            inactive mustBe Nil
        }

      "keep excluded profiles out of subsequent daily refreshes after listing and successful-update events" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- SecurityRepository.make(db)
            _        <- repo.save(List(AAPLSecurity, MSFTSecurity))
            _        <- repo.setActive(AAPL, false, ts)
            _        <- repo.updateCompanyProfileLastUpdated(List(AAPL))
            _        <- repo.reconcile(Exchange.NASDAQ, List(AAPLSecurity, MSFTSecurity))
            excluded <- readSecurity(db, AAPL)
            due      <- repo.findTickersBy(SecurityFilter.CompanyProfileNotUpdatedFor(3.days), None)
          yield
            excluded.map(_.isActive) mustBe Some(false)
            excluded.flatMap(_.exclusionReason) mustBe Some("user-excluded")
            due mustBe List(MSFT)
        }
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
            _    <- repo.setActive(inactiveTicker, false, ts)
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
            updated <- readSecurity(db, AAPL)
          yield updated.flatMap(_.companyProfileLastUpdatedAt) mustBe defined
        }

      "do nothing if security does not exist" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.updateCompanyProfileLastUpdated(List(AAPL))
            res  <- readSecurity(db, AAPL)
          yield res mustBe None
        }

      "update the companyProfileLastUpdated field for multiple tickers" in
        withEmbeddedMongoDatabase { db =>
          for
            repo        <- SecurityRepository.make(db)
            _           <- repo.save(AAPLSecurity)
            _           <- repo.save(MSFTSecurity)
            _           <- repo.updateCompanyProfileLastUpdated(List(AAPL, MSFT))
            updatedAAPL <- readSecurity(db, AAPL)
            updatedMSFT <- readSecurity(db, MSFT)
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
            res  <- readSecurity(db, AAPL)
          yield res mustBe Some(AAPLSecurity)
        }

      "update only existing securities when given mixed list" in
        withEmbeddedMongoDatabase { db =>
          val nonExistent = Ticker("NONEXIST")
          for
            repo         <- SecurityRepository.make(db)
            _            <- repo.save(AAPLSecurity)
            _            <- repo.updateCompanyProfileLastUpdated(List(AAPL, nonExistent))
            updatedAAPL  <- readSecurity(db, AAPL)
            updatedOther <- readSecurity(db, nonExistent)
          yield {
            updatedAAPL.flatMap(_.companyProfileLastUpdatedAt) mustBe defined
            updatedOther mustBe None
          }
        }
    }
  }
}
