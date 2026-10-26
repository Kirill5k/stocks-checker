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
  private val ranking = InvestmentRankingRun("published", now, recipe, Nil, Nil)
  given Clock[IO]     = Clock.mock(now)

  private def header(run: InvestmentRankingRun): InvestmentRankingRepository.Header =
    InvestmentRankingRepository.Header(run.id, run.createdAt, run.recipe, InvestmentRankingRepository.summary(run))

  "InvestmentRankingService" should {
    "reuse a ranking from the same UTC day and recipe without loading its inputs" in {
      val stocks   = mock[StockRepository[IO]]
      val history  = mock[FinancialHistoryRepository[IO]]
      val rankings = mock[InvestmentRankingRepository[IO]]
      val existing = header(ranking.copy(createdAt = Instant.parse("2026-10-19T00:00:00Z")))
      when(rankings.latestHeader).thenReturnSome(existing)
      val result = for
        service   <- InvestmentRankingService.make(stocks, history, rankings)
        refreshed <- service.refresh(date)
      yield refreshed
      result.asserting { refreshed =>
        refreshed mustBe existing
        verify(rankings).latestHeader
        verifyNoMoreInteractions(rankings)
        verifyNoInteractions(stocks, history)
        succeed
      }
    }

    "reuse the UTC day selected by the caller when the clock has moved to the next day" in {
      val stocks   = mock[StockRepository[IO]]
      val history  = mock[FinancialHistoryRepository[IO]]
      val rankings = mock[InvestmentRankingRepository[IO]]
      val existing = header(ranking.copy(createdAt = Instant.parse("2026-10-18T23:59:59Z")))
      when(rankings.latestHeader).thenReturnSome(existing)
      val result = for
        service   <- InvestmentRankingService.make(stocks, history, rankings)
        refreshed <- service.refresh(date.minusDays(1))
      yield refreshed
      result.asserting { refreshed =>
        refreshed mustBe existing
        verifyNoInteractions(stocks, history)
        succeed
      }
    }

    List(
      "no published ranking"                  -> Option.empty[InvestmentRankingRepository.Header],
      "a ranking from the previous UTC day"   -> Some(header(ranking.copy(createdAt = Instant.parse("2026-10-18T23:59:59Z")))),
      "a ranking with another recipe version" -> Some(header(ranking.copy(recipe = recipe.copy(version = "previous"))))
    ).foreach { case (condition, existing) =>
      s"publish a sorted, complete universe and prune old rankings when there is $condition" in {
        val stocks    = mock[StockRepository[IO]]
        val history   = mock[FinancialHistoryRepository[IO]]
        val rankings  = mock[InvestmentRankingRepository[IO]]
        val financial = FinancialSnapshot("facts", MSFT, "Finnhub", now, Map.empty, Nil, Nil, "", "hash", "mapping", Nil)
        when(rankings.latestHeader).thenReturn(IO.pure(existing))
        when(stocks.findAll(StockFilters(), None)).thenReturnIO(List(MSFTStock, AAPLStock))
        when(history.findLatestAll).thenReturnIO(List(financial))
        when(rankings.save(any[InvestmentRankingRun])).thenReturnUnit
        when(rankings.prune(any[Instant])).thenReturnUnit
        val result = for
          service   <- InvestmentRankingService.make(stocks, history, rankings)
          refreshed <- service.refresh(date)
        yield refreshed
        result.asserting { refreshed =>
          val saved   = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentRankingRun])
          val ordered = org.mockito.Mockito.inOrder(rankings)
          ordered.verify(rankings).save(saved.capture())
          ordered.verify(rankings).prune(now.minusSeconds(30L * 86400))
          val run = saved.getValue
          run.id must not be empty
          run.createdAt mustBe now
          run.recipe mustBe recipe
          run.inputs mustBe List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, Some(financial)))
          run.scorecards.map(_.ticker).toSet mustBe Set(AAPL, MSFT)
          run.scorecards.find(_.ticker == MSFT).flatMap(_.financialSnapshotId) mustBe Some(financial.id)
          refreshed mustBe header(run)
          succeed
        }
      }
    }

    "return a completed ranking exactly at the two-day freshness boundary" in {
      val stocks   = mock[StockRepository[IO]]
      val history  = mock[FinancialHistoryRepository[IO]]
      val rankings = mock[InvestmentRankingRepository[IO]]
      val existing = ranking.copy(createdAt = now.minusSeconds(2L * 86400))
      when(rankings.latest).thenReturnSome(existing)
      val result = for
        service <- InvestmentRankingService.make(stocks, history, rankings)
        latest  <- service.latest
      yield latest
      result.asserting { latest =>
        latest mustBe existing
        verifyNoInteractions(stocks, history)
        succeed
      }
    }

    List(
      "missing"             -> Option.empty[InvestmentRankingRun],
      "from another recipe" -> Some(ranking.copy(recipe = recipe.copy(version = "previous")))
    ).foreach { case (condition, existing) =>
      s"reject the latest ranking when it is $condition" in {
        val rankings = mock[InvestmentRankingRepository[IO]]
        when(rankings.latest).thenReturn(IO.pure(existing))
        val result = for
          service <- InvestmentRankingService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], rankings)
          latest  <- service.latest
        yield latest
        result.assertThrows(
          AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run")
        )
      }
    }

    "reject a latest ranking older than two days" in {
      val rankings = mock[InvestmentRankingRepository[IO]]
      when(rankings.latest).thenReturnSome(ranking.copy(createdAt = now.minusSeconds(2L * 86400 + 1)))
      val result = for
        service <- InvestmentRankingService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], rankings)
        latest  <- service.latest
      yield latest
      result.assertThrows(AppError.FailedValidation("The latest completed ranking is stale"))
    }

    "load a published ranking by ID" in {
      val rankings = mock[InvestmentRankingRepository[IO]]
      when(rankings.find(ranking.id)).thenReturnSome(ranking)
      val result = for
        service <- InvestmentRankingService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], rankings)
        loaded  <- service.get(ranking.id)
      yield loaded
      result.asserting(_ mustBe ranking)
    }

    "report when a published ranking disappears" in {
      val rankings = mock[InvestmentRankingRepository[IO]]
      when(rankings.find("missing")).thenReturnNone
      val result = for
        service <- InvestmentRankingService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], rankings)
        loaded  <- service.get("missing").attempt
      yield loaded
      result.asserting { loaded =>
        loaded.left.toOption.get mustBe a[IllegalStateException]
        loaded.left.toOption.get.getMessage mustBe "Published investment ranking disappeared"
      }
    }
  }
