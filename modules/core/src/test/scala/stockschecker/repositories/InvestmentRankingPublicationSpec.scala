package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

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

    "prune old unreferenced rankings and interrupted inputs while preserving screen evidence" in
      withEmbeddedMongoDatabase { db =>
        val old        = now.minusSeconds(40L * 86400)
        val referenced = ranking.copy(createdAt = old)
        val values     = List(
          referenced,
          ranking.copy(id = InvestmentRankingId("obsolete"), createdAt = old),
          ranking.copy(id = InvestmentRankingId("recent")),
          ranking.copy(id = InvestmentRankingId("interrupted"), createdAt = old),
          ranking.copy(id = InvestmentRankingId("in-flight"))
        )
        for
          repo            <- InvestmentRankingRepository.make[IO](db)
          screens         <- InvestmentScreenRepository.make[IO](db)
          _               <- values.traverse(value => repo.save(value, entries))
          _               <- screens.save(screen)
          raw             <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _               <- raw.deleteMany(Filter.in("_id", List("interrupted", "in-flight")))
          _               <- repo.prune(now.minusSeconds(30L * 86400))
          retained        <- storedRanking(db, screen.rankingId)
          retainedEntries <- storedRankingEntries(db, screen.rankingId)
          obsolete        <- storedRanking(db, InvestmentRankingId("obsolete"))
          recent          <- storedRanking(db, InvestmentRankingId("recent"))
          rawInputs       <- db.getCollection(InvestmentRankingRepository.InputsCollectionName)
          rows            <- rawInputs.find.all
          retainedRows    <- rawInputs.find(Filter.in("rankingId", List(screen.rankingId.value, "recent", "in-flight"))).all
        yield
          retained mustBe Some(referenced)
          retainedEntries mustBe entries
          obsolete mustBe None
          recent.map(_.id) mustBe Some(InvestmentRankingId("recent"))
          rows.size mustBe 3
          retainedRows.size mustBe rows.size
      }
  }
