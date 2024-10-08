package stockschecker.repositories

import cats.effect.IO
import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.fixtures.{AAPL, AAPLCompanyProfile}

import scala.concurrent.Future

class CompanyProfileRepositoryTest extends RepositorySpec {

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
            _ <- repo.save(AAPLCompanyProfile)
            _ <- repo.save(AAPLCompanyProfile)
            _ <- repo.save(AAPLCompanyProfile.copy(marketCap = 1L))
            res <- repo.find(AAPL)
          yield res.map(_.marketCap) mustBe Some(1L)
        }
      }
    }
  }
}
