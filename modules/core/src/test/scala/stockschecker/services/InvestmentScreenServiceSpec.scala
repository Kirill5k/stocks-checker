package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.repositories.*
import stockschecker.fixtures.*
import stockschecker.actions.Action
import java.time.{Instant, LocalDate}

class InvestmentScreenServiceSpec extends IOWordSpec:
  private val now = Instant.parse("2026-10-19T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  private def memoryScreens(state: Ref[IO, List[InvestmentScreenRun]]): InvestmentScreenRepository[IO] =
    new InvestmentScreenRepository[IO]:
      def save(run: InvestmentScreenRun): IO[Unit] = state.update(_ :+ run)
      def find(id: String): IO[Option[InvestmentScreenRun]] = state.get.map(_.find(_.id == id))
      def list(limit: Int): IO[List[InvestmentScreenSummary]] = state.get.map(_.reverse.take(limit).map(InvestmentScreenRepository.summary))
      def ids: IO[List[String]] = state.get.map(_.map(_.id))

  private def memoryRankings(state: Ref[IO, List[InvestmentRankingRun]]): InvestmentRankingRepository[IO] =
    new InvestmentRankingRepository[IO]:
      def save(run: InvestmentRankingRun): IO[Unit] = state.update(_ :+ run)
      def find(id: String): IO[Option[InvestmentRankingRun]] = state.get.map(_.find(_.id == id))
      def latest: IO[Option[InvestmentRankingRun]] = state.get.map(_.lastOption)

  "InvestmentScreenService" should {
    "remain disabled without both the feature switch and audited coverage" in {
      val stocks = mock[StockRepository[IO]]
      val history = mock[FinancialHistoryRepository[IO]]
      val screens = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices = mock[EvaluationPriceHistory[IO]]
      val result = for
        service <- InvestmentScreenService.make(stocks, history, screens, evaluations, prices, InvestmentScreenSettings(enabled = true), rankings = mock[InvestmentRankingRepository[IO]])
        result <- service.create(InvestmentScreenRequest()).attempt
      yield result
      result.asserting { r =>
        r.isLeft mustBe true
        verifyNoInteractions(stocks, history, screens, evaluations, prices)
        succeed
      }
    }
    "snapshot the complete universe before applying country or display limits and retain exclusions" in {
      val stocks = mock[StockRepository[IO]]
      val history = mock[FinancialHistoryRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices = mock[EvaluationPriceHistory[IO]]
      val rankingRepo = mock[InvestmentRankingRepository[IO]]
      val inputs = List(AAPLStock, MSFTStock).map(s => InvestmentCandidate(s, None))
      val ranking = InvestmentRankingRun("completed-rank", now, InvestmentRecipe(), inputs, InvestmentScorer.rank(inputs, now))
      when(rankingRepo.latest).thenReturnSome(ranking)
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        service <- InvestmentScreenService.make(stocks, history, memoryScreens(state), evaluations, prices, InvestmentScreenSettings(true, true), rankings = rankingRepo)
        created <- service.create(InvestmentScreenRequest(limit = Some(1), country = Some("GB")))
        saved <- state.get
      yield (created, saved)
      result.asserting { case (created, saved) =>
        saved.head.inputs.map(_.stock.security.ticker).toSet mustBe Set(AAPL, MSFT)
        saved.head.scorecards.size mustBe 2
        created.summary.universeCount mustBe 2
        created.summary.excludedByReason("missing-financial-snapshot") mustBe 2
        created.candidates mustBe Nil
        saved.head.rankingId mustBe ranking.id
        verifyNoInteractions(stocks, history)
        succeed
      }
    }
    "return pending evaluation for a new saved cohort without fetching nonexistent future returns" in {
      val prices = mock[EvaluationPriceHistory[IO]]
      val run = InvestmentScreenRun("today", now, InvestmentRecipe(), InvestmentScreenRequest(), Nil, Nil, List(InvestmentCohort("investment", List(AAPL))))
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]], prices, InvestmentScreenSettings(), rankings = mock[InvestmentRankingRepository[IO]])
        evaluated <- service.evaluate(run.id)
      yield evaluated
      result.asserting { report =>
        report.horizons.map(_.status) mustBe List("pending", "pending")
        verifyNoInteractions(prices)
        succeed
      }
    }
    "report near misses separately from unusable inputs and preserve the requested score floor" in {
      val cards = List(
        InvestmentScorecard(AAPL, true, Some(80), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, None, "v1"),
        InvestmentScorecard(MSFT, true, Some(60), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, None, "v1"))
      val run = InvestmentScreenRun("saved", now, InvestmentRecipe(), InvestmentScreenRequest(minScore = Some(70)),
        List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, None)), cards, Nil)
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]], mock[EvaluationPriceHistory[IO]], InvestmentScreenSettings(), rankings = mock[InvestmentRankingRepository[IO]])
        found <- service.get(run.id)
      yield found
      result.asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.nearMisses.head.failedRule must include("below required score 70")
        found.exclusions mustBe Nil
      }
    }
    "freeze a monthly top twenty independently of user display limits and only once per month" in {
      val inputs = (1 to 25).toList.map { n =>
        InvestmentCandidate(AAPLStock.copy(security = AAPLStock.security.copy(ticker = Ticker(f"TEST$n%02d"))), None)
      }
      val cards = inputs.zipWithIndex.map { case (input, n) =>
        InvestmentScorecard(input.stock.security.ticker, true, Some(BigDecimal(100 - n)),
          Map("quality" -> BigDecimal(n), "value" -> BigDecimal(100 - n)), Map.empty, Nil, Nil, Some("peer"), 25, None, "quality-fair-value-v1")
      }
      val rank = InvestmentRankingRun("published", now, InvestmentRecipe(), inputs, cards)
      val stocks = mock[StockRepository[IO]]
      val financial = mock[FinancialHistoryRepository[IO]]
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        rankingState <- Ref.of[IO, List[InvestmentRankingRun]](List(rank))
        service <- InvestmentScreenService.make(stocks, financial, memoryScreens(state), mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]], InvestmentScreenSettings(true, true), rankings = memoryRankings(rankingState))
        user <- service.create(InvestmentScreenRequest(limit = Some(1)))
        first <- service.generateScheduledScreen
        second <- service.generateScheduledScreen
        saved <- state.get
      yield (user, first, second, saved)
      result.asserting { case (user, first, second, saved) =>
        user.candidates.size mustBe 1
        user.nearMisses mustBe Nil
        user.cohorts.find(_.name == "investment").get.tickers.size mustBe 20
        val concentration = user.cohorts.find(_.name == "investment").get.concentration.get
        concentration.constituentCount mustBe 20
        concentration.maxPositionWeightPercent mustBe BigDecimal(5)
        concentration.positionHerfindahl mustBe BigDecimal("0.05")
        concentration.industryWeightsPercent.values.sum mustBe BigDecimal(100)
        first.map(_.id) mustBe Some("monthly:2026-10:quality-fair-value-v1")
        second mustBe None
        saved.size mustBe 2
        saved.forall(_.rankingId == rank.id) mustBe true
        verifyNoInteractions(stocks, financial)
        succeed
      }
    }
    "only return eligible companies failing exactly one investment criterion as near misses" in {
      val extra = Ticker("EXTRA")
      val missing = Ticker("MISSING")
      val card = InvestmentScorecard(AAPL, true, Some(90), Map("quality" -> BigDecimal(90)), Map.empty, Nil, Nil, Some("peer"), 20, None, "quality-fair-value-v1")
      val cards = List(card, card.copy(ticker = MSFT, score = Some(60)),
        card.copy(ticker = extra, score = Some(50), pillars = Map("quality" -> BigDecimal(30))),
        card.copy(ticker = missing, eligible = false, score = None, exclusions = List("missing-financial-snapshot")))
      val inputs = cards.map(c => InvestmentCandidate(AAPLStock.copy(security = AAPLStock.security.copy(ticker = c.ticker)), None))
      val run = InvestmentScreenRun("criteria", now, InvestmentRecipe(), InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70)), inputs, cards, Nil)
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]], mock[EvaluationPriceHistory[IO]], InvestmentScreenSettings(), rankings = mock[InvestmentRankingRepository[IO]])
        found <- service.get(run.id)
      yield found
      result.asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.exclusions.map(_.ticker) mustBe List(missing)
      }
    }
    "compute the full ranking once daily and avoid freezing an empty automatic cohort" in {
      val stocks = mock[StockRepository[IO]]
      val financial = mock[FinancialHistoryRepository[IO]]
      when(stocks.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock))
      when(financial.findLatestAll).thenReturnIO(Nil)
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        rankingState <- Ref.of[IO, List[InvestmentRankingRun]](Nil)
        service <- InvestmentScreenService.make(stocks, financial, memoryScreens(state), mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]], InvestmentScreenSettings(true, true), rankings = memoryRankings(rankingState))
        first <- service.generateScheduledScreen
        second <- service.generateScheduledScreen
        ranks <- rankingState.get
      yield (first, second, ranks)
      result.asserting { case (first, second, ranks) =>
        first mustBe None
        second mustBe None
        ranks.size mustBe 1
        ranks.head.inputs.size mustBe 1
        verify(stocks).findAll(StockFilters(), None)
        verify(financial).findLatestAll
        succeed
      }
    }
    "keep quota-deferred price jobs pending instead of freezing an incomplete monthly evaluation" in {
      val createdAt = Instant.parse("2026-09-01T00:00:00Z")
      val from = LocalDate.parse("2026-09-01")
      val through = LocalDate.parse("2026-10-01")
      val run = InvestmentScreenRun("durable", createdAt, InvestmentRecipe(), InvestmentScreenRequest(), Nil, Nil,
        List(InvestmentCohort("investment", List(AAPL))))
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices = mock[EvaluationPriceHistory[IO]]
      val jobs = mock[JobRepository[IO]]
      val plan = EvaluationCapturePlan(run.id, "2026-10", through, Instant.parse("2026-10-01T12:00:00Z"))
      val benchmark = EvaluationPriceSeries(InvestmentEvaluator.Benchmark,
        List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 105)), "audited", now, true, "split-and-dividend", from, through)
      when(evaluations.capturePlans).thenReturnIO(Nil)
      when(evaluations.findCapturePlan(run.id, "2026-10")).thenReturnSome(plan)
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(prices.cached(InvestmentEvaluator.Benchmark, from, through)).thenReturnSome(benchmark)
      when(prices.cached(AAPL, from, through)).thenReturnNone
      when(jobs.find(any[String])).thenReturnSome(Job("pending", Action.RefreshEvaluationPrices(AAPL, from, through), now, now))
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], memoryScreens(state),
          evaluations, prices, InvestmentScreenSettings(), rankings = mock[InvestmentRankingRepository[IO]], jobs = Some(jobs))
        report <- service.evaluate(run.id)
        saved <- service.captureMonthlyEvaluations
      yield (report, saved)
      result.asserting { case (report, saved) =>
        report.pendingDataJobs.size mustBe 1
        report.dataThrough mustBe Some(through)
        report.horizons.head.cohorts.find(_.name == "investment").get.missing mustBe List(AAPL)
        saved mustBe 0
      }
    }
    "persist an explicit missing outcome after a price job fails terminally" in {
      val createdAt = Instant.parse("2026-09-01T00:00:00Z")
      val from = LocalDate.parse("2026-09-01")
      val through = LocalDate.parse("2026-10-01")
      val run = InvestmentScreenRun("terminal", createdAt, InvestmentRecipe(), InvestmentScreenRequest(), Nil, Nil,
        List(InvestmentCohort("investment", List(AAPL))))
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices = mock[EvaluationPriceHistory[IO]]
      val jobs = mock[JobRepository[IO]]
      val benchmark = EvaluationPriceSeries(InvestmentEvaluator.Benchmark,
        List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 105)), "audited", now, true, "split-and-dividend", from, through)
      when(evaluations.capturePlans).thenReturnIO(Nil)
      when(evaluations.findCapturePlan(run.id, "2026-10")).thenReturnSome(EvaluationCapturePlan(run.id, "2026-10", through, now))
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(evaluations.saveMonthly(any[InvestmentEvaluation])).thenReturnUnit
      when(prices.cached(InvestmentEvaluator.Benchmark, from, through)).thenReturnSome(benchmark)
      when(prices.cached(AAPL, from, through)).thenReturnNone
      when(jobs.find(any[String])).thenReturnSome(Job("failed", Action.RefreshEvaluationPrices(AAPL, from, through), now, now, state = Job.Failed))
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], memoryScreens(state),
          evaluations, prices, InvestmentScreenSettings(), rankings = mock[InvestmentRankingRepository[IO]], jobs = Some(jobs))
        report <- service.evaluate(run.id)
        saved <- service.captureMonthlyEvaluations
      yield (report, saved)
      result.asserting { case (report, saved) =>
        report.pendingDataJobs mustBe Nil
        report.horizons.head.cohorts.find(_.name == "investment").get.status mustBe "incomplete"
        saved mustBe 1
      }
    }
    "materialize a deferred prior-month plan under its original capture month" in {
      val createdAt = Instant.parse("2026-08-01T00:00:00Z")
      val from = LocalDate.parse("2026-08-01")
      val through = LocalDate.parse("2026-09-01")
      val run = InvestmentScreenRun("late", createdAt, InvestmentRecipe(), InvestmentScreenRequest(), Nil, Nil,
        List(InvestmentCohort("investment", List(AAPL))))
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices = mock[EvaluationPriceHistory[IO]]
      val jobs = mock[JobRepository[IO]]
      val oldPlan = EvaluationCapturePlan(run.id, "2026-09", through, Instant.parse("2026-09-01T12:00:00Z"))
      val series = EvaluationPriceSeries(AAPL, List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 105)),
        "audited", now, true, "split-and-dividend", from, through)
      when(evaluations.capturePlans).thenReturnIO(List(oldPlan))
      when(evaluations.findMonthly(run.id, "2026-09")).thenReturnNone
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnSome(InvestmentEvaluator.evaluate(run.id, createdAt, now, run.cohorts, Map.empty))
      when(evaluations.saveMonthly(any[InvestmentEvaluation])).thenReturnUnit
      when(prices.cached(AAPL, from, through)).thenReturnSome(series)
      when(prices.cached(InvestmentEvaluator.Benchmark, from, through)).thenReturnSome(series.copy(ticker = InvestmentEvaluator.Benchmark))
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], memoryScreens(state),
          evaluations, prices, InvestmentScreenSettings(), rankings = mock[InvestmentRankingRepository[IO]], jobs = Some(jobs))
        saved <- service.captureMonthlyEvaluations
      yield saved
      result.asserting { saved =>
        val captor = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentEvaluation])
        verify(evaluations).saveMonthly(captor.capture())
        captor.getValue.captureMonth mustBe Some("2026-09")
        captor.getValue.dataThrough mustBe Some(through)
        saved mustBe 1
      }
    }
    "show one failed intrinsic financial criterion without pretending it has an investment score" in {
      val annual = (1 to 5).toList.map(n => FinancialObservation("fcfMargin", LocalDate.of(2026 - n, 12, 31),
        if n <= 2 then BigDecimal("-0.01") else BigDecimal("0.05"), "ratio"))
      val financial = FinancialSnapshot("facts", AAPL, "Finnhub", now, Map.empty, annual, Nil, "", "hash", "mapping", Nil)
      val card = InvestmentScorecard(AAPL, false, None, Map.empty, Map.empty, List("insufficient-cash-flow-durability"), Nil, None, 20, Some("facts"), "quality-fair-value-v1")
      val run = InvestmentScreenRun("intrinsic", now, InvestmentRecipe(), InvestmentScreenRequest(),
        List(InvestmentCandidate(AAPLStock, Some(financial))), List(card), Nil)
      val result = for
        state <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(mock[StockRepository[IO]], mock[FinancialHistoryRepository[IO]], memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]], mock[EvaluationPriceHistory[IO]], InvestmentScreenSettings(), rankings = mock[InvestmentRankingRepository[IO]])
        found <- service.get(run.id)
      yield found
      result.asserting { found =>
        found.candidates mustBe Nil
        found.nearMisses.head.scorecard.score mustBe None
        found.nearMisses.head.failedRule must include("3/5 positive years")
        found.exclusions.map(_.ticker) mustBe List(AAPL)
      }
    }
  }
