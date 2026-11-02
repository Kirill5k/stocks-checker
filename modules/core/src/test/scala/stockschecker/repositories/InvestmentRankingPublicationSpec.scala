package stockschecker.repositories

import cats.effect.IO
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.given

class InvestmentRankingPublicationSpec extends RepositorySpec:
  override val port = 12224

  "Published investment rankings" should {
    "ignore interrupted input writes until ranking metadata is published" in
      withEmbeddedMongoDatabase { db =>
        for
          repository  <- InvestmentRankingRepository.make[IO](db)
          _           <- repository.save(ranking, entries)
          published   <- storedRanking(db, ranking.id)
          values      <- storedRankingEntries(db, ranking.id)
          raw         <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _           <- raw.deleteOne(Filter.idEq(ranking.id.value))
          unavailable <- repository.latest
          missing     <- storedRanking(db, ranking.id)
          orphaned    <- storedRankingEntries(db, ranking.id)
        yield
          published mustBe Some(ranking)
          values mustBe entries
          unavailable mustBe None
          missing mustBe None
          orphaned mustBe entries
      }

  }
