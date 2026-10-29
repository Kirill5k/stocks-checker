package stockschecker.repositories

import cats.effect.IO
import cats.syntax.traverse.*
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

import java.time.LocalDate

class InvestmentRankingRepositorySpec extends RepositorySpec:
  override val port = 12223

  "An InvestmentRankingRepository" should {
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
          saved  <- repo.findEntries(ranking.id)
          raw    <- db.getCollection(InvestmentRankingRepository.InputsCollectionName)
          stored <- raw.find.all
        yield
          saved mustBe values
          stored.foreach { row =>
            row.keys mustBe Set("_id", "rankingId", "ordinal", "input", "scorecard", "createdAt")
            row.getDocument("input").map(_.keys) mustBe Some(
              Set("exchange", "country", "industry", "sector", "priceScore", "exclusionExplanation")
            )
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
          found  <- repo.find(published.id)
          latest <- repo.latest
          saved  <- repo.findEntries(published.id)
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
          "snapshot",
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
          card.copy(financialSnapshotId = Some(snapshot.id), evidence = Some(evidence))
        )
        for
          repo  <- InvestmentRankingRepository.make[IO](db)
          _     <- repo.save(ranking, List(value))
          saved <- repo.findEntries(ranking.id)
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
          retained       <- repo.find(referenced.id)
          retainedInputs <- repo.findEntries(referenced.id)
          removed        <- repo.find(obsolete.id)
          removedInputs  <- repo.findEntries(obsolete.id)
          atBoundary     <- repo.find(boundary.id)
          boundaryInputs <- repo.findEntries(boundary.id)
        yield
          retained mustBe Some(referenced)
          retainedInputs mustBe entries
          removed mustBe None
          removedInputs mustBe Nil
          atBoundary mustBe Some(boundary)
          boundaryInputs mustBe entries
      }

    "leave metadata unpublished if an input batch fails" in
      withEmbeddedMongoDatabase { db =>
        for
          repo   <- InvestmentRankingRepository.make[IO](db)
          failed <- repo.save(ranking, entries ++ entries).attempt
          found  <- repo.find(ranking.id)
          latest <- repo.latest
        yield
          failed.isLeft mustBe true
          found mustBe None
          latest mustBe None
      }
  }
