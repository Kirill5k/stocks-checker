package stockschecker.repositories

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
      "store security in database" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            all  <- repo.streamAll.compile.toList
          yield all mustBe List(AAPLSecurity)
        }
      }

      "store multiple securities in database" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            securities = List(AAPLSecurity, MSFTSecurity)
            _   <- repo.save(securities)
            all <- repo.streamAll.compile.toList
          yield all mustBe securities
        }
      }
    }

    "findByTicker" should {
      "find security by ticker" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            s    <- repo.findByTicker(AAPL)
          yield s mustBe Some(AAPLSecurity)
        }
      }

      "return None when security not found" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            s    <- repo.findByTicker(AAPL)
          yield s mustBe None
        }
      }
    }

    "findByExchange" should {
      "find securities by exchange" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            s    <- repo.findByExchange(Exchange.NASDAQ)
          yield s must contain theSameElementsAs List(AAPLSecurity, MSFTSecurity)
        }
      }
    }

    "getAllTickers" should {
      "return tickers of all securities stored in db" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            s    <- repo.getAllTickers
          yield s must contain theSameElementsAs List(AAPL, MSFT)
        }
      }
    }

    "streamTickersBy" should {
      "return tickers filtered by ExchangeIs" in {
        withEmbeddedMongoDatabase { db =>
          val nyseSec = AAPLSecurity.copy(ticker = Ticker("IBM"), exchange = Exchange.NYSE)
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            _    <- repo.save(nyseSec)
            res  <- repo.streamTickersBy(SecurityFilter.ExchangeIs(Exchange.NASDAQ), None).compile.toList
          yield res must contain theSameElementsAs List(AAPL, MSFT)
        }
      }

      "return tickers filtered by KindIs" in {
        withEmbeddedMongoDatabase { db =>
          val etfSec = AAPLSecurity.copy(ticker = Ticker("SPY"), kind = SecurityKind.ETF)
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(etfSec)
            res  <- repo.streamTickersBy(SecurityFilter.KindIs(SecurityKind.ETF), None).compile.toList
          yield res mustBe List(Ticker("SPY"))
        }
      }

      "return tickers filtered by IsActive" in {
        withEmbeddedMongoDatabase { db =>
          val inactiveSec = AAPLSecurity.copy(ticker = Ticker("INACTIVE"), isActive = false)
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(inactiveSec)
            res  <- repo.streamTickersBy(SecurityFilter.IsActive(true), None).compile.toList
          yield res mustBe List(AAPL)
        }
      }

      "return tickers filtered by UpdatedWithin" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTSecurity)
            res  <- repo.streamTickersBy(SecurityFilter.UpdatedWithin(50.millis), None).compile.toList
          yield res mustBe List(MSFT)
        }
      }

      "return tickers filtered by NotUpdatedFor" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- IO.sleep(100.millis)
            _    <- repo.save(MSFTSecurity)
            res  <- repo.streamTickersBy(SecurityFilter.NotUpdatedFor(50.millis), None).compile.toList
          yield res mustBe List(AAPL)
        }
      }

      "return tickers filtered by Composite filter" in {
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
            res  <- repo.streamTickersBy(composite, None).compile.toList
          yield res must contain theSameElementsAs List(AAPL, MSFT)
        }
      }

      "respect the limit parameter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            _    <- repo.save(MSFTSecurity)
            res  <- repo.streamTickersBy(SecurityFilter.ExchangeIs(Exchange.NASDAQ), Some(1)).compile.toList
          yield res.size mustBe 1
        }
      }

      "return empty list when no securities match the filter" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- SecurityRepository.make(db)
            _    <- repo.save(AAPLSecurity)
            res  <- repo.streamTickersBy(SecurityFilter.ExchangeIs(Exchange.NYSE), None).compile.toList
          yield res mustBe List.empty
        }
      }
    }
  }
}
