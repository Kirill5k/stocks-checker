package stockschecker.repositories

import cats.effect.IO
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.domain.Exchange
import stockschecker.fixtures.*

import scala.concurrent.Future

class SecurityRepositorySpec extends RepositorySpec {

  override def port: Int = 12148

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
  }
}
