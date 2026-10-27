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
  private val ranking = InvestmentRankingRun(InvestmentRankingId("published"), now, recipe, Nil, Nil)
  given Clock[IO]     = Clock.mock(now)

  private def metadata(run: InvestmentRankingRun): InvestmentRankingMetadata =
    InvestmentRankingMetadata(run.id, run.createdAt, run.recipe, Some(InvestmentRankingSummary.from(run)))

  private class Dependencies:
    val stocks   = mock[StockRepository[IO]]
    val history  = mock[FinancialHistoryRepository[IO]]
    val rankings = mock[InvestmentRankingRepository[IO]]
    val inputs   = mock[InvestmentRankingInputRepository[IO]]
    val screens  = mock[InvestmentScreenRepository[IO]]

    def service: IO[InvestmentRankingService[IO]] = InvestmentRankingService.make(stocks, history, rankings, inputs, screens)

    def allowPruning(): Unit =
      when(screens.rankingIds).thenReturnIO(Set.empty[InvestmentRankingId])
      when(rankings.obsolete(any[Instant], any[Set[InvestmentRankingId]])).thenReturnIO(Nil)
      when(inputs.delete(any[List[InvestmentRankingId]])).thenReturnUnit
      when(rankings.delete(any[List[InvestmentRankingId]])).thenReturnUnit
      when(rankings.ids).thenReturnIO(Set.empty[InvestmentRankingId])
      when(inputs.pruneOrphans(any[Set[InvestmentRankingId]], any[Instant])).thenReturnUnit
      ()

  "InvestmentRankingService" should {
    "reuse a ranking from the same UTC day and recipe without loading its inputs" in {
      val d        = new Dependencies
      val existing = ranking.copy(createdAt = Instant.parse("2026-10-19T00:00:00Z"))
      when(d.rankings.latest).thenReturnSome(metadata(existing))
      d.service.flatMap(_.refresh(date)).asserting { refreshed =>
        refreshed mustBe InvestmentRankingHeader.from(existing)
        verify(d.rankings).latest
        verifyNoMoreInteractions(d.rankings)
        verifyNoInteractions(d.stocks, d.history, d.inputs, d.screens)
        succeed
      }
    }

    "reuse the UTC day selected by the caller when the clock has moved to the next day" in {
      val d        = new Dependencies
      val existing = ranking.copy(createdAt = Instant.parse("2026-10-18T23:59:59Z"))
      when(d.rankings.latest).thenReturnSome(metadata(existing))
      d.service.flatMap(_.refresh(date.minusDays(1))).asserting { refreshed =>
        refreshed mustBe InvestmentRankingHeader.from(existing)
        verifyNoInteractions(d.stocks, d.history, d.inputs, d.screens)
        succeed
      }
    }

    List(
      "no published ranking"                  -> Option.empty[InvestmentRankingMetadata],
      "a ranking from the previous UTC day"   -> Some(metadata(ranking.copy(createdAt = Instant.parse("2026-10-18T23:59:59Z")))),
      "a ranking with another recipe version" -> Some(metadata(ranking.copy(recipe = recipe.copy(version = "previous"))))
    ).foreach { case (condition, existing) =>
      s"save the complete universe before publishing its header when there is $condition" in {
        val d         = new Dependencies
        val financial = FinancialSnapshot("facts", MSFT, "Finnhub", now, Map.empty, Nil, Nil, "", "hash", "mapping", Nil)
        when(d.rankings.latest).thenReturn(IO.pure(existing))
        when(d.stocks.findAll(StockFilters(), None)).thenReturnIO(List(MSFTStock, AAPLStock))
        when(d.history.findLatestAll).thenReturnIO(List(financial))
        when(d.inputs.save(any[InvestmentRankingRun])).thenReturnUnit
        when(d.rankings.save(any[InvestmentRankingHeader])).thenReturnUnit
        d.allowPruning()
        d.service.flatMap(_.refresh(date)).asserting { refreshed =>
          val saved   = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentRankingRun])
          val ordered = org.mockito.Mockito.inOrder(d.inputs, d.rankings, d.screens)
          ordered.verify(d.inputs).save(saved.capture())
          ordered.verify(d.rankings).save(refreshed)
          ordered.verify(d.screens).rankingIds
          ordered.verify(d.rankings).obsolete(now.minusSeconds(30L * 86400), Set.empty)
          val run = saved.getValue
          run.id.value must not be empty
          run.createdAt mustBe now
          run.recipe mustBe recipe
          run.inputs mustBe List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, Some(financial)))
          run.scorecards.map(_.ticker).toSet mustBe Set(AAPL, MSFT)
          run.scorecards.find(_.ticker == MSFT).flatMap(_.financialSnapshotId) mustBe Some(financial.id)
          refreshed mustBe InvestmentRankingHeader.from(run)
          succeed
        }
      }
    }

    "leave a ranking unpublished when saving its inputs fails" in {
      val d       = new Dependencies
      val failure = new RuntimeException("Input batch failed")
      when(d.rankings.latest).thenReturnNone
      when(d.stocks.findAll(StockFilters(), None)).thenReturnIO(List(AAPLStock))
      when(d.history.findLatestAll).thenReturnIO(Nil)
      when(d.inputs.save(any[InvestmentRankingRun])).thenReturn(IO.raiseError(failure))
      d.service.flatMap(_.refresh(date)).attempt.asserting { result =>
        result mustBe Left(failure)
        verify(d.rankings, never).save(any[InvestmentRankingHeader])
        verifyNoInteractions(d.screens)
        succeed
      }
    }

    "return stored summary metadata without reading ranking inputs" in {
      val d = new Dependencies
      when(d.rankings.latest).thenReturnSome(metadata(ranking))
      d.service.flatMap(_.latestHeader).asserting { header =>
        header mustBe Some(InvestmentRankingHeader.from(ranking))
        verifyNoInteractions(d.inputs, d.stocks, d.history, d.screens)
        succeed
      }
    }

    "backfill a legacy ranking summary without hydrating its inputs" in {
      val d       = new Dependencies
      val summary = InvestmentRankingSummary(100, 80, Map("missing-financials" -> 20))
      when(d.rankings.latest).thenReturnSome(metadata(ranking).copy(summary = None))
      when(d.inputs.summary(ranking.id)).thenReturnIO(summary)
      when(d.rankings.updateSummary(ranking.id, summary)).thenReturnUnit
      d.service.flatMap(_.latestHeader).asserting { header =>
        header mustBe Some(InvestmentRankingHeader(ranking.id, now, recipe, summary))
        verify(d.inputs).summary(ranking.id)
        verify(d.rankings).updateSummary(ranking.id, summary)
        verifyNoMoreInteractions(d.inputs)
        verifyNoInteractions(d.stocks, d.history, d.screens)
        succeed
      }
    }

    "return a completed ranking exactly at the two-day freshness boundary" in {
      val d        = new Dependencies
      val existing = ranking.copy(createdAt = now.minusSeconds(2L * 86400))
      when(d.rankings.latest).thenReturnSome(metadata(existing))
      when(d.inputs.find(existing.id)).thenReturnIO(InvestmentRankingInputs(Nil, Nil))
      d.service.flatMap(_.latest).asserting { latest =>
        latest mustBe existing
        verifyNoInteractions(d.stocks, d.history)
        succeed
      }
    }

    List(
      "missing"             -> Option.empty[InvestmentRankingMetadata],
      "from another recipe" -> Some(metadata(ranking.copy(recipe = recipe.copy(version = "previous"))))
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
      when(d.rankings.latest).thenReturnSome(metadata(ranking.copy(createdAt = now.minusSeconds(2L * 86400 + 1))))
      d.service.flatMap(_.latest).assertThrows(AppError.FailedValidation("The latest completed ranking is stale"))
    }

    "reconstruct a published ranking from its metadata and stored inputs" in {
      val d          = new Dependencies
      val candidates = List(InvestmentCandidate(MSFTStock, None), InvestmentCandidate(AAPLStock, None))
      val scorecards = InvestmentScorer.rank(candidates, now, recipe)
      val existing   = ranking.copy(inputs = candidates, scorecards = scorecards)
      when(d.rankings.find(existing.id)).thenReturnSome(metadata(existing))
      when(d.inputs.find(existing.id)).thenReturnIO(InvestmentRankingInputs(candidates, scorecards))
      d.service.flatMap(_.get(existing.id)).asserting { loaded =>
        loaded mustBe existing
        verify(d.inputs).find(existing.id)
        verifyNoMoreInteractions(d.inputs)
        succeed
      }
    }

    "return no ranking without loading orphan inputs when its header is absent" in {
      val d       = new Dependencies
      val missing = InvestmentRankingId("missing")
      when(d.rankings.find(missing)).thenReturnNone
      d.service.flatMap(_.find(missing)).asserting { loaded =>
        loaded mustBe None
        verifyNoInteractions(d.inputs)
        succeed
      }
    }

    "report when a published ranking disappears without loading orphan inputs" in {
      val d       = new Dependencies
      val missing = InvestmentRankingId("missing")
      when(d.rankings.find(missing)).thenReturnNone
      d.service.flatMap(_.get(missing)).attempt.asserting { loaded =>
        loaded.left.toOption.get mustBe a[IllegalStateException]
        loaded.left.toOption.get.getMessage mustBe "Published investment ranking disappeared"
        verifyNoInteractions(d.inputs)
        succeed
      }
    }

    "protect referenced rankings and remove obsolete inputs before headers and orphans" in {
      val d          = new Dependencies
      val before     = now.minusSeconds(30L * 86400)
      val referenced = Set(InvestmentRankingId("screen-ranking"))
      val obsolete   = List(InvestmentRankingId("obsolete"))
      val published  = referenced + ranking.id
      when(d.screens.rankingIds).thenReturnIO(referenced)
      when(d.rankings.obsolete(before, referenced)).thenReturnIO(obsolete)
      when(d.inputs.delete(obsolete)).thenReturnUnit
      when(d.rankings.delete(obsolete)).thenReturnUnit
      when(d.rankings.ids).thenReturnIO(published)
      when(d.inputs.pruneOrphans(published, before)).thenReturnUnit
      d.service.flatMap(_.prune(before)).asserting { _ =>
        val ordered = org.mockito.Mockito.inOrder(d.screens, d.rankings, d.inputs)
        ordered.verify(d.screens).rankingIds
        ordered.verify(d.rankings).obsolete(before, referenced)
        ordered.verify(d.inputs).delete(obsolete)
        ordered.verify(d.rankings).delete(obsolete)
        ordered.verify(d.rankings).ids
        ordered.verify(d.inputs).pruneOrphans(published, before)
        verifyNoInteractions(d.stocks, d.history)
        succeed
      }
    }
  }
