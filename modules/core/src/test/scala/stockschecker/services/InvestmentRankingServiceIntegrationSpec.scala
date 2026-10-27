package stockschecker.services

import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.repositories.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class InvestmentRankingServiceIntegrationSpec extends RepositorySpec:
  override val port = 12224

  given Clock[IO] = Clock.mock(now)

  private def rankingService(db: MongoDatabase[IO]): IO[InvestmentRankingService[IO]] =
    for
      stocks   <- StockRepository.make[IO](db)
      history  <- FinancialHistoryRepository.make[IO](db)
      rankings <- InvestmentRankingRepository.make[IO](db)
      inputs   <- InvestmentRankingInputRepository.make[IO](db)
      screens  <- InvestmentScreenRepository.make[IO](db)
      service  <- InvestmentRankingService.make(stocks, history, rankings, inputs, screens)
    yield service

  "An InvestmentRankingService with MongoDB repositories" should {
    "make a ranking visible only after its header is published" in
      withEmbeddedMongoDatabase { db =>
        val ranking =
          InvestmentRankingRun(InvestmentRankingId("pending-publication"), now, screenRun.recipe, screenRun.inputs, screenRun.scorecards)
        for
          service     <- rankingService(db)
          inputs      <- InvestmentRankingInputRepository.make[IO](db)
          rankings    <- InvestmentRankingRepository.make[IO](db)
          _           <- inputs.save(ranking)
          before      <- service.latestHeader
          unpublished <- service.get(ranking.id).attempt
          _           <- rankings.save(InvestmentRankingHeader.from(ranking))
          after       <- service.latestHeader
          published   <- service.get(ranking.id)
        yield
          before mustBe None
          unpublished.isLeft mustBe true
          after mustBe Some(InvestmentRankingHeader.from(ranking))
          published mustBe ranking
      }
    "backfill legacy header summaries without losing published rankings" in
      withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRankingRun(InvestmentRankingId("legacy"), now, InvestmentRecipe(), screenRun.inputs, screenRun.scorecards)
        for
          repo     <- InvestmentRankingRepository.make[IO](db)
          inputs   <- InvestmentRankingInputRepository.make[IO](db)
          service  <- rankingService(db)
          _        <- inputs.save(ranking)
          _        <- repo.save(InvestmentRankingHeader.from(ranking))
          raw      <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _        <- raw.updateOne(Filter.idEq(ranking.id.value), Update.unset("summary"))
          header   <- service.latestHeader
          byId     <- repo.find(ranking.id)
          restored <- service.get(ranking.id)
        yield
          header.map(_.id) mustBe Some(ranking.id)
          header.map(_.summary) mustBe Some(InvestmentRankingSummary(1, 1, Map.empty))
          byId.flatMap(_.summary) mustBe header.map(_.summary)
          restored mustBe ranking
      }
    "prune old unreferenced rankings and interrupted inputs while preserving screen evidence" in
      withEmbeddedMongoDatabase { db =>
        val old     = now.minusSeconds(40L * 86400)
        val ranking = InvestmentRankingRun(screenRun.rankingId, old, screenRun.recipe, screenRun.inputs, screenRun.scorecards)
        for
          repo    <- InvestmentRankingRepository.make[IO](db)
          inputs  <- InvestmentRankingInputRepository.make[IO](db)
          screens <- InvestmentScreenRepository.make[IO](db)
          service <- rankingService(db)
          published = List(
            ranking,
            ranking.copy(id = InvestmentRankingId("obsolete")),
            ranking.copy(id = InvestmentRankingId("recent"), createdAt = now)
          )
          _ <- published.traverse { value =>
            inputs.save(value).flatMap(_ => repo.save(InvestmentRankingHeader.from(value)))
          }
          _            <- screens.save(InvestmentScreenHeader.from(screenRun))
          _            <- inputs.save(ranking.copy(id = InvestmentRankingId("interrupted")))
          _            <- inputs.save(ranking.copy(id = InvestmentRankingId("in-flight"), createdAt = now))
          _            <- inputs.save(ranking.copy(id = InvestmentRankingId("legacy-orphan")))
          rawInputs    <- db.getCollection(InvestmentRankingInputRepository.CollectionName)
          _            <- rawInputs.updateOne(Filter.eq("rankingId", "legacy-orphan"), Update.unset("createdAt"))
          _            <- service.prune(now.minusSeconds(30L * 86400))
          retained     <- service.get(screenRun.rankingId)
          obsolete     <- repo.find(InvestmentRankingId("obsolete"))
          recent       <- service.get(InvestmentRankingId("recent"))
          rows         <- rawInputs.find.all
          retainedRows <- rawInputs.find(Filter.in("rankingId", List(screenRun.rankingId.value, "recent", "in-flight"))).all
        yield
          retained mustBe ranking
          obsolete mustBe None
          recent.id mustBe InvestmentRankingId("recent")
          rows.size mustBe 3
          retainedRows.size mustBe rows.size
      }
  }
