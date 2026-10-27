package stockschecker.repositories

import cats.effect.IO
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.fixtures.*

import java.time.LocalDate
import stockschecker.repositories.InvestmentRepositoryFixtures.*

class InvestmentRankingInputRepositorySpec extends RepositorySpec:
  override val port = 12222

  "An InvestmentRankingInputRepository" should {
    "store only financial diagnostics alongside the scorecard evidence" in
      withEmbeddedMongoDatabase { db =>
        val observation = FinancialObservation("fcfMargin", LocalDate.parse("2025-12-31"), BigDecimal("0.2"), "ratio")
        val snapshot    = FinancialSnapshot(
          "snapshot",
          AAPL,
          "Finnhub",
          now,
          Map("peTTM" -> BigDecimal(10), "irrelevant" -> BigDecimal(123)),
          List(observation),
          List(observation),
          "large raw provider response",
          "hash",
          "mapping",
          Nil
        )
        val candidate = InvestmentCandidate(AAPLStock, Some(snapshot))
        val evidence  = InvestmentEvidence(
          "Finnhub",
          now,
          "mapping",
          "hash",
          Map("fcfMargin" -> List(observation.period)),
          List(observation),
          None,
          None,
          Some("USD")
        )
        val ranking = InvestmentRankingRun(
          InvestmentRankingId("compact"),
          now,
          InvestmentRecipe(),
          List(candidate),
          List(card.copy(financialSnapshotId = Some(snapshot.id), evidence = Some(evidence)))
        )
        for
          repo    <- InvestmentRankingInputRepository.make[IO](db)
          _       <- repo.save(ranking)
          saved   <- repo.find(ranking.id)
          summary <- repo.summary(ranking.id)
          raw     <- db.getCollection(InvestmentRankingInputRepository.CollectionName)
          stored  <- raw.find(Filter.eq("rankingId", ranking.id.value)).first
        yield
          summary mustBe InvestmentRankingSummary(1, 1, Map.empty)
          stored must not be empty
          saved.inputs.head.financialSnapshot.get.annual mustBe Nil
          saved.inputs.head.financialSnapshot.get.quarterly mustBe Nil
          saved.inputs.head.financialSnapshot.get.rawPayload mustBe ""
          saved.inputs.head.financialSnapshot.get.metrics mustBe Map("peTTM" -> BigDecimal(10))
          saved.scorecards mustBe ranking.scorecards
      }
    "preserve input order independently from scorecard order" in
      withEmbeddedMongoDatabase { db =>
        val ranking = InvestmentRankingRun(
          InvestmentRankingId("different-orders"),
          now,
          screenRun.recipe,
          List(InvestmentCandidate(MSFTStock, None), InvestmentCandidate(AAPLStock, None)),
          List(card, card.copy(ticker = MSFT))
        )
        for
          repo  <- InvestmentRankingInputRepository.make[IO](db)
          _     <- repo.save(ranking)
          saved <- repo.find(ranking.id)
        yield
          saved.inputs mustBe ranking.inputs
          saved.scorecards mustBe ranking.scorecards
      }
  }
