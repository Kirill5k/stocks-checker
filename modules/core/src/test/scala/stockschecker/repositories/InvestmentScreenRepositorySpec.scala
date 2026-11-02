package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class InvestmentScreenRepositorySpec extends RepositorySpec:
  override val port = 12220

  "An InvestmentScreenRepository" should {
    "hide a complete screen before limiting when any cohort holding becomes unavailable and restore it without rewriting history" in
      withActiveStocks(List(AAPL, MSFT)) { db =>
        val affected = screen.copy(
          id = InvestmentScreenId("affected"),
          createdAt = now.plusSeconds(1),
          cohorts = screen.cohorts :+ InvestmentCohort(CohortName.EqualUniverse, List(AAPL, MSFT)),
          summary = screen.summary.copy(id = InvestmentScreenId("affected"), createdAt = now.plusSeconds(1))
        )
        for
          repo          <- InvestmentScreenRepository.make[IO](db)
          rankings      <- InvestmentRankingRepository.make[IO](db)
          securities    <- db.getCollection(SecurityRepository.CollectionName)
          profiles      <- db.getCollection(CompanyProfileRepository.CollectionName)
          _             <- rankings.save(ranking, entries)
          _             <- List(screen, affected).traverse(repo.save)
          before        <- repo.list(1)
          _             <- securities.updateOne(Filter.idEq(MSFT.value), Update.set("isActive", false))
          hidden        <- repo.list(1)
          unchanged     <- repo.findWithRanking(affected.id)
          retained      <- repo.get(affected.id)
          _             <- securities.updateOne(Filter.idEq(MSFT.value), Update.set("isActive", true))
          _             <- profiles.updateOne(Filter.idEq(MSFT.value), Update.set("isActive", false))
          profileHidden <- repo.list(1)
          _             <- profiles.updateOne(Filter.idEq(MSFT.value), Update.set("isActive", true))
          restored      <- repo.findWithRanking(affected.id)
          restoredList  <- repo.list(1)
        yield
          before.map(_.id) mustBe List(affected.id)
          hidden.map(_.id) mustBe List(screen.id)
          unchanged mustBe (affected -> ranking)
          retained mustBe Some(affected)
          profileHidden.map(_.id) mustBe List(screen.id)
          restored mustBe (affected -> ranking)
          restoredList.map(_.id) mustBe List(affected.id)
      }

    "return current visible ranking tickers without changing frozen entries" in
      withActiveStocks(List(AAPL, MSFT)) { db =>
        val values = entries :+ entries.head.copy(scorecard = card.copy(ticker = MSFT))
        for
          repo       <- InvestmentScreenRepository.make[IO](db)
          rankings   <- InvestmentRankingRepository.make[IO](db)
          securities <- db.getCollection(SecurityRepository.CollectionName)
          _          <- rankings.save(ranking, values)
          visible    <- repo.visibleRankingTickers(ranking.id)
          selected   <- repo.visibleRankingTickers(ranking.id, Some(Set(MSFT, Ticker("MISSING"))))
          empty      <- repo.visibleRankingTickers(ranking.id, Some(Set.empty))
          _          <- securities.updateOne(Filter.idEq(MSFT.value), Update.set("listingStatus", ListingStatus.Delisted.print))
          current    <- repo.visibleRankingTickers(ranking.id)
          hidden     <- repo.visibleRankingTickers(ranking.id, Some(Set(MSFT)))
          frozen     <- repo.rankingEntries(ranking.id)
        yield
          visible mustBe Set(AAPL, MSFT)
          selected mustBe Set(MSFT)
          empty mustBe Set.empty
          current mustBe Set(AAPL)
          hidden mustBe Set.empty
          frozen mustBe values
      }

    "read stored cohort entries without loading unrelated exclusions unless explicitly requested" in
      withActiveStocks(List(AAPL, MSFT)) { db =>
        val excluded = (1 to 1000).toList.map { n =>
          entries.head.copy(scorecard =
            card.copy(ticker = Ticker(s"EXCLUDED$n"), eligible = false, exclusions = List("missing-financial-snapshot"))
          )
        }
        val otherHolding = entries.head.copy(scorecard = card.copy(ticker = MSFT))
        val values       = excluded.take(500) ++ List(otherHolding) ++ excluded.drop(500) ++ entries
        val saved        = screen.copy(cohorts =
          List(
            InvestmentCohort(CohortName.Investment, List(AAPL)),
            InvestmentCohort(CohortName.EqualUniverse, List(AAPL, MSFT))
          )
        )
        for
          repo     <- InvestmentScreenRepository.make[IO](db)
          rankings <- InvestmentRankingRepository.make[IO](db)
          _        <- rankings.save(ranking, values)
          _        <- rankings.save(ranking.copy(id = InvestmentRankingId("another-ranking")), entries)
          bounded  <- repo.detailEntries(saved, includeExclusions = false)
          full     <- repo.detailEntries(saved, includeExclusions = true)
          frozen   <- repo.rankingEntries(ranking.id)
        yield
          bounded mustBe List(otherHolding, entries.head)
          full mustBe values
          frozen mustBe values
      }

    "filter financial near misses by scope and current availability before limiting them in ranking order" in {
      val inactive        = (1 to InvestmentRecipe.NearMissLimit).toList.map(n => Ticker(s"INACTIVE$n"))
      val profileInactive = Ticker("PROFILE-INACTIVE")
      val delisted        = Ticker("DELISTED")
      val foreign         = Ticker("FOREIGN")
      val otherExchange   = Ticker("OTHER-EXCHANGE")
      val eligible        = Ticker("ELIGIBLE")
      val unexplained     = Ticker("UNEXPLAINED")
      val nearTickers     = (1 to (InvestmentRecipe.NearMissLimit + 2)).toList.reverse.map(n => Ticker(f"NEAR$n%02d"))
      val tickers = AAPL :: (inactive ++ List(profileInactive, delisted, foreign, otherExchange, eligible, unexplained) ++ nearTickers)
      def nearMiss(ticker: Ticker): InvestmentRankingEntry =
        entries.head.copy(
          input = entries.head.input
            .copy(exchange = Exchange.NASDAQ, country = Some("US"), exclusionExplanation = Some("one financial rule failed")),
          scorecard = card.copy(ticker = ticker, eligible = false, score = None, exclusions = List("insufficient-cash-flow-durability"))
        )
      val filtered = List(
        nearMiss(foreign).copy(input = nearMiss(foreign).input.copy(country = Some("GB"))),
        nearMiss(otherExchange).copy(input = nearMiss(otherExchange).input.copy(exchange = Exchange.NYSE)),
        nearMiss(eligible).copy(scorecard = card.copy(ticker = eligible)),
        nearMiss(unexplained).copy(input = nearMiss(unexplained).input.copy(exclusionExplanation = None))
      ) ++ (inactive ++ List(profileInactive, delisted)).map(nearMiss)
      val nearby = nearTickers.map(nearMiss)
      val values = filtered ++ nearby.take(2) ++ entries ++ nearby.drop(2)
      val saved  = screen.copy(request = InvestmentScreenRequest(exchange = Some(Exchange.NASDAQ), country = Some("US")))
      withActiveStocks(tickers) { db =>
        for
          repo       <- InvestmentScreenRepository.make[IO](db)
          rankings   <- InvestmentRankingRepository.make[IO](db)
          securities <- db.getCollection(SecurityRepository.CollectionName)
          profiles   <- db.getCollection(CompanyProfileRepository.CollectionName)
          _          <- rankings.save(ranking, values)
          _          <- securities.updateMany(
            Filter.in(SecurityRepository.Field.Id, inactive.map(_.value)),
            Update.set(SecurityRepository.Field.IsActive, false)
          )
          _ <- securities.updateOne(
            Filter.idEq(delisted.value),
            Update.set(SecurityRepository.Field.ListingStatus, ListingStatus.Delisted.print)
          )
          _       <- profiles.updateOne(Filter.idEq(profileInactive.value), Update.set(CompanyProfileRepository.Field.IsActive, false))
          bounded <- repo.detailEntries(saved, includeExclusions = false)
        yield bounded mustBe (nearby.take(2) ++ entries ++ nearby.slice(2, InvestmentRecipe.NearMissLimit))
      }
    }

    "join a screen to its pinned ranking while loading ordered entries separately" in
      withActiveStocks(List(AAPL, MSFT)) { db =>
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
      withActiveStocks(List(AAPL, MSFT)) { db =>
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
      withActiveStocks(List(AAPL, MSFT)) { db =>
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
      withActiveStocks(List(AAPL, MSFT)) { db =>
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
      withActiveStocks(List(AAPL, MSFT)) { db =>
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
      withActiveStocks(List(AAPL, MSFT)) { db =>
        val expected = screen
        for
          repo  <- InvestmentScreenRepository.make[IO](db)
          _     <- repo.save(expected)
          saved <- repo.get(screen.id)
        yield saved mustBe Some(expected)
      }
  }
