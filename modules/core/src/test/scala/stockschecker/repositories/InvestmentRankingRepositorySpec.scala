package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import mongo4cats.bson.BsonValue
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

import java.time.LocalDate

class InvestmentRankingRepositorySpec extends RepositorySpec with MongoJsonCodecs:
  override val port = 12223

  "An InvestmentRankingRepository" should {
    "findCandidates" should {
      "retain joined analytics with missing or null basis in stocks and research candidates" in {
        val msftAnalytics = AAPLPriceAnalytics.copy(ticker = MSFT)
        val aapl          = AAPLStock.copy(priceAnalytics = Some(AAPLPriceAnalytics.copy(basis = None)))
        val msft          = MSFTStock.copy(
          profile = None,
          priceAnalytics = Some(msftAnalytics.copy(basis = None)),
          financialMetrics = None
        )
        val seedData = Map(
          SecurityRepository.CollectionName       -> List(toDoc(AAPLSecurity), toDoc(MSFTSecurity)),
          CompanyProfileRepository.CollectionName -> List(toDoc(AAPLCompanyProfile)),
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
            InvestmentCandidate(AAPLStock, Some(latest)),
            InvestmentCandidate(MSFTStock.copy(profile = None, priceAnalytics = None, financialMetrics = None), None)
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
            row.keys mustBe Set("_id", "rankingId", "ordinal", "input", "scorecard", "createdAt")
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

    "prune only old rankings without a referencing screen and remove their inputs" in
      withEmbeddedMongoDatabase { db =>
        val before     = now.minusSeconds(3L * 86400)
        val referenced = ranking.copy(createdAt = before.minusSeconds(1))
        val obsolete   = ranking.copy(id = InvestmentRankingId("obsolete"), createdAt = before.minusSeconds(1))
        val boundary   = ranking.copy(id = InvestmentRankingId("boundary"), createdAt = before)
        for
          repo           <- InvestmentRankingRepository.make[IO](db)
          screens        <- InvestmentScreenRepository.make[IO](db)
          _              <- List(referenced, obsolete, boundary).traverse(value => repo.save(value, entries))
          _              <- screens.save(screen)
          _              <- screens.save(screen.copy(id = InvestmentScreenId("second-reference")))
          _              <- repo.prune(before)
          retained       <- storedRanking(db, referenced.id)
          retainedInputs <- storedRankingEntries(db, referenced.id)
          removed        <- storedRanking(db, obsolete.id)
          removedInputs  <- storedRankingEntries(db, obsolete.id)
          atBoundary     <- storedRanking(db, boundary.id)
          boundaryInputs <- storedRankingEntries(db, boundary.id)
        yield
          retained mustBe Some(referenced)
          retainedInputs mustBe entries
          removed mustBe None
          removedInputs mustBe Nil
          atBoundary mustBe Some(boundary)
          boundaryInputs mustBe entries
      }

    "sweep only old orphan input groups while retaining published, boundary and in-flight inputs" in
      withEmbeddedMongoDatabase { db =>
        val before    = now.minusSeconds(3L * 86400)
        val pinned    = ranking.copy(createdAt = before.minusSeconds(1))
        val oldOrphan = ranking.copy(id = InvestmentRankingId("old-orphan"), createdAt = before.minusSeconds(1))
        val boundary  = ranking.copy(id = InvestmentRankingId("boundary-orphan"), createdAt = before)
        val inFlight  = ranking.copy(id = InvestmentRankingId("in-flight-orphan"))
        for
          repo       <- InvestmentRankingRepository.make[IO](db)
          screens    <- InvestmentScreenRepository.make[IO](db)
          _          <- List(pinned, oldOrphan, boundary, inFlight).traverse(value => repo.save(value, entries))
          _          <- screens.save(screen)
          headers    <- db.getCollection(InvestmentRankingRepository.CollectionName)
          _          <- headers.deleteMany(Filter.in("_id", List(oldOrphan.id.value, boundary.id.value, inFlight.id.value)))
          _          <- repo.prune(before)
          published  <- storedRankingEntries(db, pinned.id)
          removed    <- storedRankingEntries(db, oldOrphan.id)
          atBoundary <- storedRankingEntries(db, boundary.id)
          recent     <- storedRankingEntries(db, inFlight.id)
        yield
          published mustBe entries
          removed mustBe Nil
          atBoundary mustBe entries
          recent mustBe entries
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
  }
