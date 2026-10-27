package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.*

import java.time.{Instant, LocalDate}

class InvestmentRankingServiceSpec extends IOWordSpec:
  private val now     = Instant.parse("2026-10-19T12:00:00Z")
  private val date    = LocalDate.parse("2026-10-19")
  private val recipe  = InvestmentRecipe()
  private val ranking = InvestmentRanking(InvestmentRankingId("published"), now, recipe, InvestmentRankingSummary(0, 0, Map.empty))
  given Clock[IO]     = Clock.mock(now)

  private class Dependencies:
    val stocks   = mock[StockRepository[IO]]
    val history  = mock[FinancialHistoryRepository[IO]]
    val rankings = mock[InvestmentRankingRepository[IO]]
    val screens  = mock[InvestmentScreenRepository[IO]]

    def service: IO[InvestmentRankingService[IO]] = InvestmentRankingService.make(stocks, history, rankings, screens)

    def allowPruning(): Unit =
      when(screens.rankingIds).thenReturnIO(Set.empty[InvestmentRankingId])
      when(rankings.prune(any[Instant], any[Set[InvestmentRankingId]])).thenReturnUnit
      ()

  "InvestmentRankingService" should {
    "reuse a ranking from the same UTC day and recipe without loading its entries" in {
      val d        = new Dependencies
      val existing = ranking.copy(createdAt = Instant.parse("2026-10-19T00:00:00Z"))
      when(d.rankings.latest).thenReturnSome(existing)
      d.service.flatMap(_.refresh(date)).asserting { refreshed =>
        refreshed mustBe existing
        verify(d.rankings).latest
        verifyNoMoreInteractions(d.rankings)
        verifyNoInteractions(d.stocks, d.history, d.screens)
        succeed
      }
    }

    "reuse the UTC day selected by the caller when the clock has moved to the next day" in {
      val d        = new Dependencies
      val existing = ranking.copy(createdAt = Instant.parse("2026-10-18T23:59:59Z"))
      when(d.rankings.latest).thenReturnSome(existing)
      d.service.flatMap(_.refresh(date.minusDays(1))).asserting { refreshed =>
        refreshed mustBe existing
        verifyNoInteractions(d.stocks, d.history, d.screens)
        succeed
      }
    }

    List(
      "no published ranking"                  -> Option.empty[InvestmentRanking],
      "a ranking from the previous UTC day"   -> Some(ranking.copy(createdAt = Instant.parse("2026-10-18T23:59:59Z"))),
      "a ranking with another recipe version" -> Some(ranking.copy(recipe = recipe.copy(version = "previous")))
    ).foreach { case (condition, existing) =>
      s"score the complete universe and compact its inputs before storage when there is $condition" in {
        val d           = new Dependencies
        val observation = FinancialObservation("fcfMargin", date.minusYears(1), BigDecimal("0.2"), "ratio")
        val financial   = FinancialSnapshot(
          "facts",
          MSFT,
          Provider.Finnhub,
          now,
          Map("peTTM" -> BigDecimal(10), "pfcfShareTTM" -> BigDecimal(12), "epsTTM" -> BigDecimal(4), "irrelevant" -> BigDecimal(999)),
          List(observation),
          List(observation),
          "raw provider response",
          "hash",
          "mapping",
          Nil
        )
        when(d.rankings.latest).thenReturn(IO.pure(existing))
        when(d.stocks.findAll(StockFilters(), None)).thenReturnIO(List(MSFTStock, AAPLStock))
        when(d.history.findLatestAll).thenReturnIO(List(financial))
        when(d.rankings.save(any[InvestmentRanking], any[List[InvestmentRankingEntry]])).thenReturnUnit
        d.allowPruning()
        d.service.flatMap(_.refresh(date)).asserting { refreshed =>
          val captured = org.mockito.ArgumentCaptor.forClass(classOf[List[InvestmentRankingEntry]])
          val ordered  = org.mockito.Mockito.inOrder(d.rankings, d.screens)
          ordered.verify(d.rankings).save(org.mockito.ArgumentMatchers.eq(refreshed), captured.capture())
          ordered.verify(d.screens).rankingIds
          ordered.verify(d.rankings).prune(now.minusSeconds(30L * 86400), Set.empty)
          val saved = captured.getValue
          refreshed.id.value must not be empty
          refreshed.createdAt mustBe now
          refreshed.recipe mustBe recipe
          val compactFinancial = financial.copy(
            metrics = Map("peTTM" -> BigDecimal(10), "pfcfShareTTM" -> BigDecimal(12), "epsTTM" -> BigDecimal(4)),
            annual = Nil,
            quarterly = Nil,
            rawPayload = ""
          )
          saved.map(_.candidate) mustBe List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, Some(compactFinancial)))
          saved.find(_.scorecard.ticker == MSFT).flatMap(_.scorecard.financialSnapshotId) mustBe Some(financial.id)
          refreshed.summary mustBe InvestmentRankingSummary.from(saved)
          succeed
        }
      }
    }

    "skip pruning when saving the ranking fails" in {
      val d       = new Dependencies
      val failure = new RuntimeException("Input batch failed")
      when(d.rankings.latest).thenReturnNone
      when(d.stocks.findAll(StockFilters(), None)).thenReturnIO(List(AAPLStock))
      when(d.history.findLatestAll).thenReturnIO(Nil)
      when(d.rankings.save(any[InvestmentRanking], any[List[InvestmentRankingEntry]])).thenReturn(IO.raiseError(failure))
      d.service.flatMap(_.refresh(date)).attempt.asserting { result =>
        result mustBe Left(failure)
        verifyNoInteractions(d.screens)
        succeed
      }
    }

    "return stored metadata without loading entries" in {
      val d = new Dependencies
      when(d.rankings.latest).thenReturnSome(ranking)
      d.service.flatMap(_.latestAvailable).asserting { result =>
        result mustBe Some(ranking)
        verify(d.rankings).latest
        verifyNoMoreInteractions(d.rankings)
        verifyNoInteractions(d.stocks, d.history, d.screens)
        succeed
      }
    }

    "return a completed ranking exactly at the two-day freshness boundary" in {
      val d        = new Dependencies
      val existing = ranking.copy(createdAt = now.minusSeconds(2L * 86400))
      when(d.rankings.latest).thenReturnSome(existing)
      d.service.flatMap(_.latest).asserting(_ mustBe existing)
    }

    List(
      "missing"             -> Option.empty[InvestmentRanking],
      "from another recipe" -> Some(ranking.copy(recipe = recipe.copy(version = "previous")))
    ).foreach { case (condition, existing) =>
      s"reject the latest ranking when it is $condition" in {
        val d = new Dependencies
        when(d.rankings.latest).thenReturn(IO.pure(existing))
        d.service
          .flatMap(_.latest)
          .assertThrows(
            AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run")
          )
      }
    }

    "reject a latest ranking older than two days" in {
      val d = new Dependencies
      when(d.rankings.latest).thenReturnSome(ranking.copy(createdAt = now.minusSeconds(2L * 86400 + 1)))
      d.service.flatMap(_.latest).assertThrows(AppError.FailedValidation("The latest completed ranking is stale"))
    }

    "load ranking metadata and entries independently" in {
      val d     = new Dependencies
      val entry = InvestmentRankingEntry(InvestmentCandidate(AAPLStock, None), InvestmentRepositoryFixtures.card)
      when(d.rankings.find(ranking.id)).thenReturnSome(ranking)
      when(d.rankings.findEntries(ranking.id)).thenReturnIO(List(entry))
      (for
        service  <- d.service
        metadata <- service.get(ranking.id)
        entries  <- service.entries(ranking.id)
      yield (metadata, entries)).asserting { case (metadata, entries) =>
        metadata mustBe ranking
        entries mustBe List(entry)
      }
    }

    "report when published metadata disappears" in {
      val d       = new Dependencies
      val missing = InvestmentRankingId("missing")
      when(d.rankings.find(missing)).thenReturnNone
      d.service.flatMap(_.get(missing)).attempt.asserting { loaded =>
        loaded.left.toOption.get mustBe a[IllegalStateException]
        loaded.left.toOption.get.getMessage mustBe "Published investment ranking disappeared"
      }
    }

    "pass referenced rankings to repository retention" in {
      val d          = new Dependencies
      val before     = now.minusSeconds(30L * 86400)
      val referenced = Set(InvestmentRankingId("screen-ranking"))
      when(d.screens.rankingIds).thenReturnIO(referenced)
      when(d.rankings.prune(before, referenced)).thenReturnUnit
      d.service.flatMap(_.prune(before)).asserting { _ =>
        verify(d.rankings).prune(before, referenced)
        verifyNoInteractions(d.stocks, d.history)
        succeed
      }
    }
  }
