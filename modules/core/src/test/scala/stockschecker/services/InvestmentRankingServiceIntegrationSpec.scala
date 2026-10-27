package stockschecker.services

import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.Filter
import stockschecker.actions.ActionDispatcher
import stockschecker.domain.*
import stockschecker.repositories.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class InvestmentRankingServiceIntegrationSpec extends RepositorySpec:
  override val port = 12224

  given Clock[IO] = Clock.mock(now)

  private def rankingService(db: MongoDatabase[IO]): IO[InvestmentRankingService[IO]] =
    for
      stocks       <- StockRepository.make[IO](db)
      rankings     <- InvestmentRankingRepository.make[IO](db)
      jobs         <- JobRepository.make[IO](db)
      dispatcher   <- ActionDispatcher.make(jobs)
      stockService <- StockService.make(stocks, dispatcher)
      service      <- InvestmentRankingService.make(stockService, rankings)
    yield service

  "An InvestmentRankingService with MongoDB repositories" should {
    "ignore interrupted input writes until ranking metadata is published" in
      withEmbeddedMongoDatabase { db =>
        for
          service     <- rankingService(db)
          repository  <- InvestmentRankingRepository.make[IO](db)
          _           <- repository.save(ranking, entries)
          published   <- service.find(ranking.id)
          values      <- service.entries(ranking.id)
          raw         <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _           <- raw.deleteOne(Filter.idEq(ranking.id.value))
          unavailable <- service.latestAvailable
          missing     <- service.find(ranking.id)
          orphaned    <- repository.findEntries(ranking.id)
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
          service         <- rankingService(db)
          _               <- values.traverse(value => repo.save(value, entries))
          _               <- screens.save(screen)
          raw             <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _               <- raw.deleteMany(Filter.in("_id", List("interrupted", "in-flight")))
          _               <- service.prune(now.minusSeconds(30L * 86400))
          retained        <- service.find(screen.rankingId)
          retainedEntries <- service.entries(screen.rankingId)
          obsolete        <- repo.find(InvestmentRankingId("obsolete"))
          recent          <- service.find(InvestmentRankingId("recent"))
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
