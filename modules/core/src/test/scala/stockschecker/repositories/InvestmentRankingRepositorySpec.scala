package stockschecker.repositories

import cats.effect.IO
import mongo4cats.operations.Filter
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.repositories.InvestmentRepositoryFixtures.*

import java.time.LocalDate

class InvestmentRankingRepositorySpec extends RepositorySpec:
  override val port = 12223

  "An InvestmentRankingRepository" should {
    "preserve published metadata and ordered entries in the existing collections" in
      withEmbeddedMongoDatabase { db =>
        val values = List(
          InvestmentRankingEntry(InvestmentCandidate(MSFTStock, None), card.copy(ticker = MSFT)),
          InvestmentRankingEntry(InvestmentCandidate(AAPLStock, None), card)
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

    "preserve supplied candidates and scorecard evidence without transforming them" in
      withEmbeddedMongoDatabase { db =>
        val observation = FinancialObservation("fcfMargin", LocalDate.parse("2025-12-31"), BigDecimal("0.2"), "ratio")
        val snapshot    = FinancialSnapshot(
          "snapshot",
          AAPL,
          Provider.Finnhub,
          now,
          Map("peTTM" -> BigDecimal(10), "irrelevant" -> BigDecimal(123)),
          List(observation),
          List(observation),
          "raw provider response",
          "hash",
          "mapping",
          Nil
        )
        val evidence = InvestmentEvidence(
          Provider.Finnhub,
          now,
          "mapping",
          "hash",
          Map("fcfMargin" -> List(observation.period)),
          List(observation),
          None,
          None,
          Some("USD")
        )
        val value = InvestmentRankingEntry(
          InvestmentCandidate(AAPLStock, Some(snapshot)),
          card.copy(financialSnapshotId = Some(snapshot.id), evidence = Some(evidence))
        )
        for
          repo  <- InvestmentRankingRepository.make[IO](db)
          _     <- repo.save(ranking, List(value))
          saved <- repo.findEntries(ranking.id)
        yield saved mustBe List(value)
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
