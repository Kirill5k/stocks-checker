package stockschecker.repositories

import cats.effect.IO
import kirill5k.common.cats.Clock
import scala.concurrent.duration.FiniteDuration
import mongo4cats.bson.BsonValue
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.given

import java.time.LocalDate

class InvestmentRankingRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  override val port = 12223

  "An InvestmentRankingRepository" should {
    "findCandidates" should {
      "exclude inactive, delisted and missing-profile stocks before decoding their financial snapshots" in {
        val missing         = Ticker("MISSING")
        val inactiveProfile = Ticker("PROFILE")
        val delisted        = Ticker("DELISTED")
        val seedData        = Map(
          SecurityRepository.CollectionName -> List(
            toDoc(AAPLSecurity),
            toDoc(MSFTSecurity.copy(isActive = false)),
            toDoc(AAPLSecurity.copy(ticker = missing)),
            toDoc(AAPLSecurity.copy(ticker = delisted, listingStatus = ListingStatus.Delisted))
          ),
          CompanyProfileRepository.CollectionName -> List(
            toDoc(AAPLCompanyProfile),
            toDoc(MSFTCompanyProfile),
            toDoc(AAPLCompanyProfile.copy(ticker = delisted))
          ),
          FinancialHistoryRepository.CollectionName -> List(MSFT, missing, inactiveProfile, delisted).map(ticker =>
            Document("_id" := ticker.value, "invalid" := true)
          )
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            repo       <- InvestmentRankingRepository.make[IO](db)
            candidates <- repo.findCandidates
          yield candidates.map(_.stock.security.ticker) mustBe List(AAPL)
        }
      }

      "retain joined analytics with missing or null basis in stocks and research candidates" in {
        val msftAnalytics = AAPLPriceAnalytics.copy(ticker = MSFT)
        val aapl          = AAPLStock.copy(priceAnalytics = Some(AAPLPriceAnalytics.copy(basis = None)))
        val msft          = MSFTStock.copy(
          profile = MSFTCompanyProfile,
          priceAnalytics = Some(msftAnalytics.copy(basis = None)),
          financialMetrics = None
        )
        val seedData = Map(
          SecurityRepository.CollectionName       -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          CompanyProfileRepository.CollectionName -> List(toDoc(AAPLCompanyProfile), toDoc(MSFTCompanyProfile)),
          PriceAnalyticsRepository.CollectionName -> List(
            toDoc(AAPLPriceAnalytics).remove("basis"),
            toDoc(msftAnalytics).add("basis", BsonValue.Null)
          ),
          FinancialMetricsRepository.CollectionName -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            repo       <- InvestmentRankingRepository.make[IO](db)
            candidates <- repo.findCandidates
          yield candidates mustBe List(InvestmentCandidate(aapl, None), InvestmentCandidate(msft, None))
        }
      }

      "join the canonical financial snapshot by ticker without mixing other tickers" in {
        val observedAt = java.time.Instant.parse("2026-10-19T12:00:00Z")
        val latest     = FinancialSnapshot(
          AAPL,
          Provider.Finnhub,
          observedAt.plusMillis(1),
          Map(FinancialMetric.RevenueGrowth5Y -> BigDecimal(100)),
          Nil,
          Nil,
          "latest-hash",
          "canonical-test-v1",
          Nil
        )
        val other    = latest.copy(ticker = Ticker("OTHER"), observedAt = observedAt.plusSeconds(60), payloadHash = "other-hash")
        val seedData = Map(
          SecurityRepository.CollectionName         -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          CompanyProfileRepository.CollectionName   -> List(toDoc(AAPLCompanyProfile)),
          PriceAnalyticsRepository.CollectionName   -> List(toDoc(AAPLPriceAnalytics)),
          FinancialMetricsRepository.CollectionName -> List(toDoc(AAPLFinancialMetrics))
        )
        withEmbeddedMongoDatabase(seedData) { db =>
          for
            history <- FinancialHistoryRepository.make[IO](db)
            _       <- history.save(latest)
            _       <- history.save(other)
            repo    <- InvestmentRankingRepository.make[IO](db)
            result  <- repo.findCandidates
          yield result mustBe List(
            InvestmentCandidate(AAPLStock, Some(latest))
          )
        }
      }

      "return no candidates when there are no securities" in
        withEmbeddedMongoDatabase(Map.empty) { db =>
          for
            repo   <- InvestmentRankingRepository.make[IO](db)
            result <- repo.findCandidates
          yield result mustBe Nil
        }
    }

    "store only screen selection fields instead of copying stock profiles and analytics" in
      withEmbeddedMongoDatabase { db =>
        val withoutBasis = AAPLStock.copy(priceAnalytics = Some(AAPLPriceAnalytics.copy(basis = None)))
        val withBasis    = MSFTStock.copy(priceAnalytics = Some(AAPLPriceAnalytics.copy(ticker = MSFT)))
        val values       = List(
          InvestmentRankingEntry.from(InvestmentCandidate(withoutBasis, None), card),
          InvestmentRankingEntry.from(InvestmentCandidate(withBasis, None), card.copy(ticker = MSFT))
        )
        for
          repo   <- InvestmentRankingRepository.make[IO](db)
          _      <- repo.save(ranking, values)
          saved  <- storedRankingEntries(db, ranking.id)
          raw    <- db.getCollection(InvestmentRankingRepository.InputsCollectionName)
          stored <- raw.find.all
        yield
          saved mustBe values
          stored.foreach { row =>
            row.keys mustBe Set("_id", "rankingId", "ordinal", "input", "scorecard", "createdAt", "expiresAt")
            row.getDocument("input").map(_.keys) mustBe Some(
              Set("exchange", "country", "industry", "sector", "priceScore", "exclusionExplanation")
            )
            row.getDocument("scorecard").map(_.keys.contains("financialSnapshotId")) mustBe Some(false)
          }
          succeed
      }

    "preserve published metadata and ordered entries in the existing collections" in
      withEmbeddedMongoDatabase { db =>
        val values = List(
          InvestmentRankingEntry.from(InvestmentCandidate(MSFTStock, None), card.copy(ticker = MSFT)),
          InvestmentRankingEntry.from(InvestmentCandidate(AAPLStock, None), card)
        )
        val published = ranking.copy(summary = InvestmentRankingSummary.from(values))
        for
          repo   <- InvestmentRankingRepository.make[IO](db)
          _      <- repo.save(published, values)
          found  <- storedRanking(db, published.id)
          latest <- repo.latest
          saved  <- storedRankingEntries(db, published.id)
          raw    <- db.getCollection(InvestmentRankingRepository.InputsCollectionName)
          stored <- raw.find(Filter.eq("rankingId", published.id.value)).all
        yield
          found mustBe Some(published)
          latest mustBe Some(published)
          saved mustBe values
          stored.size mustBe 2
      }

    "preserve compact selection inputs and complete scorecard evidence" in
      withEmbeddedMongoDatabase { db =>
        val observation = FinancialObservation(FinancialMetric.FreeCashFlowMargin, LocalDate.parse("2025-12-31"), BigDecimal("0.2"))
        val snapshot    = FinancialSnapshot(
          AAPL,
          Provider.Finnhub,
          now,
          Map(FinancialMetric.PriceToEarningsTtm -> BigDecimal(10), FinancialMetric.RevenueGrowth5Y -> BigDecimal(123)),
          List(observation),
          List(observation),
          "hash",
          "mapping",
          Nil
        )
        val evidence = InvestmentEvidence(
          Provider.Finnhub,
          now,
          "mapping",
          "hash",
          Map(FinancialMetric.FreeCashFlowMargin -> List(observation.period)),
          List(observation),
          None,
          None,
          Some("USD")
        )
        val value = InvestmentRankingEntry.from(
          InvestmentCandidate(AAPLStock, Some(snapshot)),
          card.copy(evidence = Some(evidence))
        )
        for
          repo  <- InvestmentRankingRepository.make[IO](db)
          _     <- repo.save(ranking, List(value))
          saved <- storedRankingEntries(db, ranking.id)
        yield saved mustBe List(value)
      }

    "store expiry as BSON dates and pin both the header and its inputs" in withEmbeddedMongoDatabase { db =>
      for
        repo        <- InvestmentRankingRepository.make[IO](db)
        _           <- repo.save(ranking, entries)
        headers     <- db.getCollection(InvestmentRankingRepository.CollectionName)
        inputs      <- db.getCollection(InvestmentRankingRepository.InputsCollectionName)
        header      <- headers.find.first
        input       <- inputs.find.first
        _           <- repo.pin(ranking.id)
        pinned      <- headers.find.first
        pinnedInput <- inputs.find.first
      yield
        val expiry = now.plusSeconds(3L * 86400)
        header.flatMap(_.get("expiresAt").flatMap(_.asInstant)) mustBe Some(expiry)
        input.flatMap(_.get("expiresAt").flatMap(_.asInstant)) mustBe Some(expiry)
        pinned.exists(_.keys.contains("expiresAt")) mustBe false
        pinnedInput.exists(_.keys.contains("expiresAt")) mustBe false
    }

    "release only unreferenced rankings and retain the original retention boundary" in withEmbeddedMongoDatabase { db =>
      for
        repo          <- InvestmentRankingRepository.make[IO](db)
        screens       <- InvestmentScreenRepository.make[IO](db)
        _             <- repo.save(ranking, entries)
        _             <- screens.save(screen)
        _             <- repo.pin(ranking.id)
        _             <- repo.release(ranking.id, now)
        headers       <- db.getCollection(InvestmentRankingRepository.CollectionName)
        inputs        <- db.getCollection(InvestmentRankingRepository.InputsCollectionName)
        referenced    <- headers.find.first
        _             <- screens.delete(AAPL)
        _             <- repo.release(ranking.id, now)
        released      <- headers.find.first
        releasedInput <- inputs.find.first
      yield
        referenced.exists(_.keys.contains("expiresAt")) mustBe false
        released.flatMap(_.get("expiresAt").flatMap(_.asInstant)) mustBe Some(now.plusSeconds(3L * 86400))
        releasedInput.flatMap(_.get("expiresAt").flatMap(_.asInstant)) mustBe Some(now.plusSeconds(3L * 86400))
    }

    "re-pin after a screen and concurrent pin appear between the reference check and release writes" in withEmbeddedMongoDatabase { db =>
      for
        screens <- InvestmentScreenRepository.make[IO](db)
        headers <- db.getCollection(InvestmentRankingRepository.CollectionName)
        inputs  <- db.getCollection(InvestmentRankingRepository.InputsCollectionName)
        concurrentClock = new Clock[IO]:
          def now: IO[java.time.Instant] =
            screens.save(screen) >>
              inputs.updateMany(Filter.eq("rankingId", ranking.id.value), Update.unset("expiresAt")).void >>
              headers.updateOne(Filter.idEq(ranking.id.value), Update.unset("expiresAt")).as(InvestmentRepositoryFixtures.now)
          def durationBetweenNowAnd(other: java.time.Instant): IO[FiniteDuration] = IO.pure(scala.concurrent.duration.Duration.Zero)
          def sleep(duration: FiniteDuration): IO[Unit]                           = IO.unit
        repo   <- InvestmentRankingRepository.make[IO](db)(using summon[cats.MonadThrow[IO]], concurrentClock)
        _      <- repo.save(ranking, entries)
        _      <- repo.release(ranking.id, now)
        header <- headers.find.first
        input  <- inputs.find.first
      yield
        header.exists(_.keys.contains("expiresAt")) mustBe false
        input.exists(_.keys.contains("expiresAt")) mustBe false
    }

    "skip expired ranking headers in latest and paginated input reads before TTL removes them" in withEmbeddedMongoDatabase { db =>
      for
        repo         <- InvestmentRankingRepository.make[IO](db)
        screens      <- InvestmentScreenRepository.make[IO](db)
        _            <- repo.save(ranking, entries)
        headers      <- db.getCollection(InvestmentRankingRepository.CollectionName)
        _            <- headers.updateOne(Filter.idEq(ranking.id.value), Update.set("expiresAt", now.minusMillis(1)))
        latest       <- repo.latest
        screenLatest <- screens.latestRanking
        page         <- repo.rankingInputs(ranking.id, RankingInputsQuery(limit = 50)).attempt
        stored       <- storedRanking(db, ranking.id)
      yield
        latest mustBe None
        screenLatest mustBe None
        page mustBe Left(AppError.EntityDoesNotExist("Investment ranking", ranking.id.value))
        stored mustBe Some(ranking)
    }

    "purge a ticker's input rows from every ranking without deleting other inputs or headers" in withEmbeddedMongoDatabase { db =>
      val values = entries :+ entries.head.copy(scorecard = card.copy(ticker = MSFT))
      val second = ranking.copy(id = InvestmentRankingId("second"))
      for
        repo       <- InvestmentRankingRepository.make[IO](db)
        _          <- repo.save(ranking, values)
        _          <- repo.save(second, values)
        _          <- repo.delete(AAPL)
        _          <- repo.delete(AAPL)
        firstRows  <- storedRankingEntries(db, ranking.id)
        secondRows <- storedRankingEntries(db, second.id)
        header     <- storedRanking(db, ranking.id)
      yield
        firstRows.map(_.scorecard.ticker) mustBe List(MSFT)
        secondRows.map(_.scorecard.ticker) mustBe List(MSFT)
        header mustBe Some(ranking)
    }

    "leave metadata unpublished if an input batch fails" in
      withEmbeddedMongoDatabase { db =>
        for
          repo   <- InvestmentRankingRepository.make[IO](db)
          failed <- repo.save(ranking, entries ++ entries).attempt
          found  <- storedRanking(db, ranking.id)
          latest <- repo.latest
        yield
          failed.isLeft mustBe true
          found mustBe None
          latest mustBe None
      }
    "rankingInputs" should {
      "page frozen ranking inputs by ordinal with canonical industry and sector filters" in withEmbeddedMongoDatabase { db =>
        val base     = InvestmentRepositoryFixtures.entries.head
        val evidence = InvestmentEvidence(Provider.Finnhub, now, "mapping-v1", "payload-hash", Map.empty, Nil, None, None, Some("USD"))
        val rows     = List(
          ("FIRST", "  HEALTH CARE  ", Sector.Healthcare),
          ("OTHER", "Technology", Sector.Technology),
          ("SECOND", "Health Care", Sector.Healthcare),
          ("OTHER-SECTOR", "health care", Sector.Industrials)
        ).map { case (ticker, industry, sector) =>
          base.copy(
            input = base.input.copy(industry = Some(InvestmentScreenInput.industryKey(industry)), sector = Some(sector)),
            scorecard = base.scorecard
              .copy(ticker = Ticker(ticker), eligible = false, exclusions = List("missing-metric:eps-growth5y"), evidence = Some(evidence))
          )
        }
        val ranking = InvestmentRanking(InvestmentRankingId("paged-ranking"), now, InvestmentRecipe(), InvestmentRankingSummary.from(rows))
        val query   = RankingInputsQuery(Some(" HeALth CaRe "), Some(Sector.Healthcare), limit = 1)
        for
          rankings  <- InvestmentRankingRepository.make[IO](db)
          _         <- rankings.save(ranking, rows)
          _         <- rankings.save(ranking.copy(id = InvestmentRankingId("newer-ranking"), createdAt = now.plusSeconds(1)), rows)
          first     <- rankings.rankingInputs(ranking.id, query)
          second    <- rankings.rankingInputs(ranking.id, query.copy(after = first.nextAfter))
          exhausted <- rankings.rankingInputs(ranking.id, query.copy(after = Some(2)))
          literal   <- rankings.rankingInputs(ranking.id, query.copy(industry = Some(".*")))
          all       <- rankings.rankingInputs(ranking.id, RankingInputsQuery(limit = 50))
          stored    <- InvestmentRepositoryFixtures.storedRankingEntries(db, ranking.id)
        yield
          first.rankingId mustBe ranking.id
          first.asOf mustBe now
          first.items.map(_.ticker) mustBe List(Ticker("FIRST"))
          first.nextAfter mustBe Some(0)
          first.items.head.industry mustBe Some("health care")
          first.items.head.exclusions mustBe List("missing-metric:eps-growth5y")
          first.items.head.evidence mustBe Some(evidence)
          second.items.map(_.ordinal) mustBe List(2)
          second.items.map(_.ticker) mustBe List(Ticker("SECOND"))
          second.nextAfter mustBe None
          exhausted.items mustBe Nil
          exhausted.nextAfter mustBe None
          literal.items mustBe Nil
          all.items.map(_.ordinal) mustBe List(0, 1, 2, 3)
          stored mustBe rows
      }

      "distinguish missing ranking metadata from an empty published ranking or unmatched input page" in withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRanking(InvestmentRankingId("empty-ranking"), now, InvestmentRecipe(), InvestmentRankingSummary.from(Nil))
        for
          rankings <- InvestmentRankingRepository.make[IO](db)
          _        <- rankings.save(ranking, Nil)
          missing  <- rankings.rankingInputs(InvestmentRankingId("unknown"), RankingInputsQuery(limit = 50)).attempt
          empty    <- rankings.rankingInputs(ranking.id, RankingInputsQuery(Some("unmatched"), limit = 50))
        yield
          missing mustBe Left(AppError.EntityDoesNotExist("Investment ranking", "unknown"))
          empty mustBe RankingInputsPage(ranking.id, now, Nil, None)
      }

    }
  }
