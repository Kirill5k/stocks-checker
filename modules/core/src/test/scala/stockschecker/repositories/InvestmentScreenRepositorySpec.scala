package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class InvestmentScreenRepositorySpec extends RepositorySpec:
  override val port = 12220

  "An InvestmentScreenRepository" should {
    "join a screen to its pinned ranking while loading ordered entries separately" in
      withEmbeddedMongoDatabase { db =>
        val values    = List(entries.head.copy(scorecard = card.copy(ticker = MSFT)), entries.head)
        val published = ranking.copy(summary = InvestmentRankingSummary.from(values))
        val newer     = ranking.copy(id = InvestmentRankingId("newer"), createdAt = now.plusSeconds(1))
        for
          repo     <- InvestmentScreenRepository.make[IO](db)
          rankings <- InvestmentRankingRepository.make[IO](db)
          _        <- rankings.save(published, values)
          _        <- rankings.save(newer, Nil)
          _        <- repo.save(screen)
          joined   <- repo.findWithRanking(screen.id)
          latest   <- repo.latestRanking
          ordered  <- repo.rankingEntries(published.id)
          missing  <- repo.rankingEntries(InvestmentRankingId("missing"))
        yield
          joined mustBe (screen -> published)
          latest mustBe Some(newer)
          ordered mustBe values
          missing mustBe Nil
      }

    "reject joined reads when the screen or its pinned ranking is missing" in
      withEmbeddedMongoDatabase { db =>
        val missingId = InvestmentScreenId("missing")
        for
          repo     <- InvestmentScreenRepository.make[IO](db)
          missing  <- repo.findWithRanking(missingId).attempt
          _        <- repo.save(screen)
          dangling <- repo.findWithRanking(screen.id).attempt
          latest   <- repo.latestRanking
        yield
          missing mustBe Left(AppError.EntityDoesNotExist("Investment screen", missingId.value))
          dangling mustBe Left(AppError.EntityDoesNotExist("Investment screen", screen.id.value))
          latest mustBe None
      }

    "round trip screen metadata and reject overwriting a published screen" in
      withEmbeddedMongoDatabase { db =>
        val expected = screen
        for
          repo      <- InvestmentScreenRepository.make[IO](db)
          _         <- repo.save(expected)
          overwrite <- repo.save(expected.copy(cohorts = Nil)).attempt
          header    <- repo.get(screen.id)
          listed    <- repo.list(20)
          raw       <- db.getCollection(InvestmentScreenRepository.CollectionName)
          stored    <- raw
            .find(
              Filter.idEq(screen.id.value) && Filter.eq("summary.id", screen.id.value) && Filter
                .eq("rankingId", screen.rankingId.value)
            )
            .first
        yield
          overwrite.isLeft mustBe true
          header mustBe Some(expected)
          header.map(_.id) mustBe Some(screen.id)
          header.map(_.rankingId) mustBe Some(screen.rankingId)
          listed.map(_.id) mustBe List(screen.id)
          stored must not be empty
      }
    "read existing UUID and monthly screen IDs stored as BSON strings" in
      withEmbeddedMongoDatabase { db =>
        val ids = List("e7ce580d-1cdf-4219-9d34-1a25437f05b1", "monthly:2026-10:quality-fair-value-v1").map(InvestmentScreenId(_))
        for
          repo <- InvestmentScreenRepository.make[IO](db)
          raw  <- db.getCollection(InvestmentScreenRepository.CollectionName)
          _    <- raw.insertMany(ids.map { id =>
            Document(
              "_id"       := id.value,
              "createdAt" := now,
              "rankingId" := screen.rankingId.value,
              "request"   := Document(),
              "cohorts"   := List(Document("name" := "investment", "tickers" := List(AAPL.value))),
              "summary"   := Document(
                "id"               := id.value,
                "createdAt"        := now.toString,
                "modelVersion"     := ranking.recipe.version,
                "universeCount"    := 1,
                "eligibleCount"    := 1,
                "selectedCount"    := 1,
                "excludedByReason" := Document()
              )
            )
          })
          saved  <- ids.traverse(repo.get)
          listed <- repo.list(20)
        yield
          saved mustBe ids.map(id => Some(screen.copy(id = id, summary = screen.summary.copy(id = id))))
          listed.map(_.id).toSet mustBe ids.toSet
      }
    "return no metadata when the screen header does not exist" in
      withEmbeddedMongoDatabase { db =>
        val id = InvestmentScreenId("unfinished")
        for
          repo   <- InvestmentScreenRepository.make[IO](db)
          saved  <- repo.get(id)
          listed <- repo.list(20)
        yield
          saved mustBe None
          listed mustBe Nil
      }
    "read screen metadata independently of ranking storage" in
      withEmbeddedMongoDatabase { db =>
        val expected = screen
        for
          repo  <- InvestmentScreenRepository.make[IO](db)
          _     <- repo.save(expected)
          saved <- repo.get(screen.id)
        yield saved mustBe Some(expected)
      }
  }
