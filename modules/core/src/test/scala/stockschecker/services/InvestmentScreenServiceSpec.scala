package stockschecker.services

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.common.config.InvestmentConfig
import stockschecker.repositories.*
import stockschecker.fixtures.*
import stockschecker.actions.Action
import java.time.{Instant, LocalDate}

class InvestmentScreenServiceSpec extends IOWordSpec:
  private val now = Instant.parse("2026-10-19T12:00:00Z")
  private def investmentConfig(enabled: Boolean = false, coverageAudited: Boolean = false): InvestmentConfig =
    InvestmentConfig(enabled, coverageAudited, false, 7, 14, 180, 550)
  given Clock[IO] = Clock.mock(now)

  private def memoryScreens(state: Ref[IO, List[InvestmentScreenRun]]): InvestmentScreenRepository[IO] =
    new InvestmentScreenRepository[IO]:
      def save(run: InvestmentScreenRun): IO[Unit]                                      = state.update(_ :+ run)
      def find(id: InvestmentScreenId): IO[Option[InvestmentScreenRun]]                 = state.get.map(_.find(_.id == id))
      def header(id: InvestmentScreenId): IO[Option[InvestmentScreenRepository.Header]] = find(id).map(
        _.map(r =>
          InvestmentScreenRepository.Header(r.id, r.createdAt, r.rankingId, r.request, r.cohorts, InvestmentScreenRepository.summary(r))
        )
      )
      def list(limit: Int): IO[List[InvestmentScreenSummary]] = state.get.map(_.reverse.take(limit).map(InvestmentScreenRepository.summary))
      def ids: IO[List[InvestmentScreenId]]                   = state.get.map(_.map(_.id))

  private def memoryRankings(state: Ref[IO, List[InvestmentRankingRun]]): InvestmentRankingRepository[IO] =
    new InvestmentRankingRepository[IO]:
      def save(run: InvestmentRankingRun): IO[Unit]                          = state.update(_ :+ run)
      def prune(before: Instant): IO[Unit]                                   = IO.unit
      def find(id: String): IO[Option[InvestmentRankingRun]]                 = state.get.map(_.find(_.id == id))
      def latest: IO[Option[InvestmentRankingRun]]                           = state.get.map(_.lastOption)
      def header(id: String): IO[Option[InvestmentRankingRepository.Header]] =
        find(id).map(_.map(r => InvestmentRankingRepository.Header(r.id, r.createdAt, r.recipe, InvestmentRankingRepository.summary(r))))
      def latestHeader: IO[Option[InvestmentRankingRepository.Header]] = latest.flatMap(_.traverse(r => header(r.id)).map(_.flatten))

  "InvestmentScreenService" should {
    "skip the complete research refresh unless both feature flags are enabled" in {
      val stocks      = mock[StockRepository[IO]]
      val history     = mock[FinancialHistoryRepository[IO]]
      val screens     = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val rankings    = mock[InvestmentRankingRepository[IO]]
      val jobs        = mock[JobRepository[IO]]
      val result      = List((false, false), (false, true), (true, false)).traverse_ { case (enabled, audited) =>
        InvestmentScreenService
          .make(stocks, history, screens, evaluations, prices, investmentConfig(enabled, audited), rankings = rankings, jobs = jobs)
          .flatMap(_.refreshResearch)
      }
      result.asserting { _ =>
        verifyNoInteractions(stocks, history, screens, evaluations, prices, rankings, jobs)
        succeed
      }
    }
    "publish the daily ranking before capturing monthly evaluations during a research refresh" in {
      val stocks      = mock[StockRepository[IO]]
      val history     = mock[FinancialHistoryRepository[IO]]
      val screens     = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val rankings    = mock[InvestmentRankingRepository[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(rankings.latestHeader).thenReturnNone
      when(stocks.findAll(StockFilters(), None)).thenReturnIO(List(AAPLStock))
      when(history.findLatestAll).thenReturnIO(Nil)
      when(rankings.save(any[InvestmentRankingRun])).thenReturnUnit
      when(rankings.prune(any[Instant])).thenReturnUnit
      when(screens.header(InvestmentScreenId("monthly:2026-10:quality-fair-value-v1"))).thenReturnNone
      when(screens.ids).thenReturnIO(Nil)
      when(evaluations.capturePlans).thenReturnIO(Nil)
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      val result = for
        service <- InvestmentScreenService.make(
          stocks,
          history,
          screens,
          evaluations,
          prices,
          investmentConfig(true, true),
          rankings = rankings,
          jobs = jobs
        )
        _ <- service.refreshResearch
      yield ()
      result.asserting { _ =>
        val ordered = org.mockito.Mockito.inOrder(rankings, screens, evaluations)
        val ranking = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentRankingRun])
        ordered.verify(rankings).save(ranking.capture())
        ordered.verify(screens).ids
        ordered.verify(evaluations).finishedScreenIds
        ranking.getValue.inputs.map(_.stock.security.ticker) mustBe List(AAPL)
        verifyNoInteractions(prices, jobs)
        succeed
      }
    }
    "propagate ranking failures and skip evaluation capture during a research refresh" in {
      val stocks      = mock[StockRepository[IO]]
      val history     = mock[FinancialHistoryRepository[IO]]
      val screens     = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val rankings    = mock[InvestmentRankingRepository[IO]]
      val jobs        = mock[JobRepository[IO]]
      val error       = new IllegalStateException("Ranking storage is unavailable")
      when(rankings.latestHeader).thenReturn(IO.raiseError(error))
      val result = for
        service <- InvestmentScreenService.make(
          stocks,
          history,
          screens,
          evaluations,
          prices,
          investmentConfig(true, true),
          rankings = rankings,
          jobs = jobs
        )
        outcome <- service.refreshResearch.attempt
      yield outcome
      result.asserting { outcome =>
        outcome mustBe Left(error)
        verifyNoInteractions(stocks, history, screens, evaluations, prices, jobs)
        succeed
      }
    }
    "remain disabled without both the feature switch and audited coverage" in {
      val stocks      = mock[StockRepository[IO]]
      val history     = mock[FinancialHistoryRepository[IO]]
      val screens     = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val result      = for
        service <- InvestmentScreenService.make(
          stocks,
          history,
          screens,
          evaluations,
          prices,
          investmentConfig(enabled = true),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = mock[JobRepository[IO]]
        )
        result <- service.create(InvestmentScreenRequest()).attempt
      yield result
      result.asserting { r =>
        r.isLeft mustBe true
        verifyNoInteractions(stocks, history, screens, evaluations, prices)
        succeed
      }
    }
    "snapshot the complete universe before applying country or display limits and retain exclusions" in {
      val stocks      = mock[StockRepository[IO]]
      val history     = mock[FinancialHistoryRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val rankingRepo = mock[InvestmentRankingRepository[IO]]
      val inputs      = List(AAPLStock, MSFTStock).map(s => InvestmentCandidate(s, None))
      val ranking     = InvestmentRankingRun("completed-rank", now, InvestmentRecipe(), inputs, InvestmentScorer.rank(inputs, now))
      when(rankingRepo.latest).thenReturnSome(ranking)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        service <- InvestmentScreenService.make(
          stocks,
          history,
          memoryScreens(state),
          evaluations,
          prices,
          investmentConfig(true, true),
          rankings = rankingRepo,
          jobs = mock[JobRepository[IO]]
        )
        created <- service.create(InvestmentScreenRequest(limit = Some(1), country = Some("GB")))
        saved   <- state.get
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
      val prices      = mock[EvaluationPriceHistory[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      when(evaluations.findMonthly(InvestmentScreenId("today"), "2026-10")).thenReturnNone
      val run = InvestmentScreenRun(
        InvestmentScreenId("today"),
        now,
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        Nil,
        Nil,
        List(InvestmentCohort("investment", List(AAPL)))
      )
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          evaluations,
          prices,
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = mock[JobRepository[IO]]
        )
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
        InvestmentScorecard(MSFT, true, Some(60), Map.empty, Map.empty, Nil, Nil, Some("peer"), 20, None, "v1")
      )
      val run = InvestmentScreenRun(
        InvestmentScreenId("saved"),
        now,
        InvestmentRecipe(),
        InvestmentScreenRequest(minScore = Some(70)),
        List(InvestmentCandidate(AAPLStock, None), InvestmentCandidate(MSFTStock, None)),
        cards,
        Nil
      )
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]],
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = mock[JobRepository[IO]]
        )
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
        InvestmentScorecard(
          input.stock.security.ticker,
          true,
          Some(BigDecimal(100 - n)),
          Map("quality" -> BigDecimal(n), "value" -> BigDecimal(100 - n)),
          Map.empty,
          Nil,
          Nil,
          Some("peer"),
          25,
          None,
          "quality-fair-value-v1"
        )
      }
      val rank      = InvestmentRankingRun("published", now, InvestmentRecipe(), inputs, cards)
      val stocks    = mock[StockRepository[IO]]
      val financial = mock[FinancialHistoryRepository[IO]]
      val result    = for
        state        <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        rankingState <- Ref.of[IO, List[InvestmentRankingRun]](List(rank))
        service      <- InvestmentScreenService.make(
          stocks,
          financial,
          memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]],
          investmentConfig(true, true),
          rankings = memoryRankings(rankingState),
          jobs = mock[JobRepository[IO]]
        )
        user   <- service.create(InvestmentScreenRequest(limit = Some(1)))
        first  <- service.generateScheduledScreen
        second <- service.generateScheduledScreen
        saved  <- state.get
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
        first.map(_.id) mustBe Some(InvestmentScreenId("monthly:2026-10:quality-fair-value-v1"))
        second mustBe None
        saved.size mustBe 2
        saved.forall(_.rankingId == rank.id) mustBe true
        verifyNoInteractions(stocks, financial)
        succeed
      }
    }
    "only return eligible companies failing exactly one investment criterion as near misses" in {
      val extra   = Ticker("EXTRA")
      val missing = Ticker("MISSING")
      val card    = InvestmentScorecard(
        AAPL,
        true,
        Some(90),
        Map("quality" -> BigDecimal(90)),
        Map.empty,
        Nil,
        Nil,
        Some("peer"),
        20,
        None,
        "quality-fair-value-v1"
      )
      val cards = List(
        card,
        card.copy(ticker = MSFT, score = Some(60)),
        card.copy(ticker = extra, score = Some(50), pillars = Map("quality" -> BigDecimal(30))),
        card.copy(ticker = missing, eligible = false, score = None, exclusions = List("missing-financial-snapshot"))
      )
      val inputs = cards.map(c => InvestmentCandidate(AAPLStock.copy(security = AAPLStock.security.copy(ticker = c.ticker)), None))
      val run    = InvestmentScreenRun(
        InvestmentScreenId("criteria"),
        now,
        InvestmentRecipe(),
        InvestmentScreenRequest(minScore = Some(70), minQuality = Some(70)),
        inputs,
        cards,
        Nil
      )
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]],
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = mock[JobRepository[IO]]
        )
        found <- service.get(run.id)
      yield found
      result.asserting { found =>
        found.candidates.map(_.ticker) mustBe List(AAPL)
        found.nearMisses.map(_.scorecard.ticker) mustBe List(MSFT)
        found.exclusions.map(_.ticker) mustBe List(missing)
      }
    }
    "compute the full ranking once daily and avoid freezing an empty automatic cohort" in {
      val stocks    = mock[StockRepository[IO]]
      val financial = mock[FinancialHistoryRepository[IO]]
      when(stocks.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock))
      when(financial.findLatestAll).thenReturnIO(Nil)
      val result = for
        state        <- Ref.of[IO, List[InvestmentScreenRun]](Nil)
        rankingState <- Ref.of[IO, List[InvestmentRankingRun]](Nil)
        service      <- InvestmentScreenService.make(
          stocks,
          financial,
          memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]],
          investmentConfig(true, true),
          rankings = memoryRankings(rankingState),
          jobs = mock[JobRepository[IO]]
        )
        first  <- service.generateScheduledScreen
        second <- service.generateScheduledScreen
        ranks  <- rankingState.get
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
    "keep failed ticker and benchmark jobs retryable without freezing an incomplete month" in {
      val createdAt = Instant.parse("2026-09-01T00:00:00Z")
      val from      = LocalDate.parse("2026-09-01")
      val through   = LocalDate.parse("2026-09-30")
      val run       = InvestmentScreenRun(
        InvestmentScreenId("retryable"),
        createdAt,
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        Nil,
        Nil,
        List(InvestmentCohort("investment", List(AAPL)))
      )
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      val plan        = EvaluationCapturePlan(run.id, "2026-10", through, now)
      when(evaluations.capturePlans).thenReturnIO(List(plan))
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(evaluations.getOrCreateCapturePlan(any[EvaluationCapturePlan])).thenReturnIO(plan)
      when(evaluations.findCapturePlan(run.id, "2026-10")).thenReturnSome(plan)
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(prices.cached(any[Ticker], any[LocalDate], any[LocalDate])).thenReturnNone
      when(jobs.find(any[String]))
        .thenReturnSome(Job("failed", Action.RefreshEvaluationPrices(AAPL, from, through), now, now, state = Job.Failed))
      when(jobs.retryTerminal(any[String], any[Instant])).thenReturnIO(true)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          evaluations,
          prices,
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = jobs
        )
        report <- service.evaluate(run.id)
        saved  <- service.captureMonthlyEvaluations
      yield (report, saved)
      result.asserting { case (report, saved) =>
        report.pendingDataJobs mustBe Nil
        report.settled mustBe Some(false)
        report.dataThrough mustBe Some(through)
        saved mustBe 0
        verify(jobs, times(2)).retryTerminal(any[String], any[Instant])
        verify(evaluations, never).saveMonthly(any[InvestmentEvaluation])
        verify(prices, never).history(any[Ticker], any[LocalDate], any[LocalDate])
        succeed
      }
    }
    "recalculate an incomplete saved month using screen metadata and preserve the original cutoff" in {
      val createdAt = Instant.parse("2026-08-01T00:00:00Z")
      val from      = LocalDate.parse("2026-08-01")
      val through   = LocalDate.parse("2026-09-01")
      val run       = InvestmentScreenRun(
        InvestmentScreenId("recover"),
        createdAt,
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        Nil,
        Nil,
        List(InvestmentCohort("investment", List(AAPL)))
      )
      val header =
        InvestmentScreenRepository.Header(run.id, createdAt, "ranking", run.request, run.cohorts, InvestmentScreenRepository.summary(run))
      val screens     = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      val oldPlan     = EvaluationCapturePlan(run.id, "2026-09", through, Instant.parse("2026-09-01T12:00:00Z"))
      val incomplete  =
        InvestmentEvaluator.evaluate(run.id, createdAt, now, run.cohorts, Map.empty, Some(through)).copy(captureMonth = Some("2026-09"))
      val series = EvaluationPriceSeries(
        AAPL,
        List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 105)),
        "audited",
        now,
        true,
        "split-and-dividend",
        DateRange(from, through)
      )
      when(screens.ids).thenReturnIO(Nil)
      when(screens.header(run.id)).thenReturnSome(header)
      when(evaluations.capturePlans).thenReturnIO(Nil)
      when(evaluations.getOrCreateCapturePlan(any[EvaluationCapturePlan])).thenReturnIO(oldPlan)
      when(evaluations.unsettled).thenReturnIO(List(incomplete))
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(evaluations.findMonthly(run.id, "2026-09")).thenReturnSome(incomplete)
      when(evaluations.saveMonthly(any[InvestmentEvaluation])).thenReturnUnit
      when(prices.cached(AAPL, from, through)).thenReturnSome(series)
      when(prices.cached(InvestmentEvaluator.Benchmark, from, through)).thenReturnSome(series.copy(ticker = InvestmentEvaluator.Benchmark))
      val result = for
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          screens,
          evaluations,
          prices,
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = jobs
        )
        saved <- service.captureMonthlyEvaluations
      yield saved
      result.asserting { saved =>
        val captor = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentEvaluation])
        verify(evaluations).saveMonthly(captor.capture())
        val planCaptor = org.mockito.ArgumentCaptor.forClass(classOf[EvaluationCapturePlan])
        verify(evaluations).getOrCreateCapturePlan(planCaptor.capture())
        planCaptor.getValue.month mustBe "2026-09"
        planCaptor.getValue.through mustBe through
        captor.getValue.captureMonth mustBe Some("2026-09")
        captor.getValue.dataThrough mustBe Some(through)
        InvestmentEvaluationRepository.complete(captor.getValue) mustBe true
        saved mustBe 1
        verify(screens, never).find(any[InvestmentScreenId])
        verifyNoInteractions(jobs)
        succeed
      }
    }
    "share durable price jobs across screens created on different dates" in {
      val through = LocalDate.parse("2026-09-30")
      val screens = List("2026-08-01", "2026-09-01").zipWithIndex.map { case (day, i) =>
        InvestmentScreenRun(
          InvestmentScreenId(s"screen-$i"),
          Instant.parse(s"${day}T00:00:00Z"),
          InvestmentRecipe(),
          InvestmentScreenRequest(),
          Nil,
          Nil,
          List(InvestmentCohort("investment", List(AAPL)))
        )
      }
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(evaluations.findMonthly(any[InvestmentScreenId], any[String])).thenReturnNone
      when(evaluations.capturePlans).thenReturnIO(Nil)
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(evaluations.getOrCreateCapturePlan(any[EvaluationCapturePlan])).thenAnswer(invocation =>
        IO.pure(invocation.getArgument[EvaluationCapturePlan](0))
      )
      when(prices.cached(any[Ticker], any[LocalDate], any[LocalDate])).thenReturnNone
      when(jobs.find(any[String])).thenReturnNone
      when(jobs.enqueue(any[Job])).thenReturnUnit
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](screens)
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          evaluations,
          prices,
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = jobs
        )
        saved <- service.captureMonthlyEvaluations
      yield saved
      result.asserting { saved =>
        saved mustBe 0
        val captor = org.mockito.ArgumentCaptor.forClass(classOf[Job])
        verify(jobs, times(4)).enqueue(captor.capture())
        import scala.jdk.CollectionConverters.*
        captor.getAllValues.asScala.map(_.id).distinct.size mustBe 2
        captor.getAllValues.asScala.map(_.action).toSet mustBe Set(
          Action.RefreshEvaluationPrices(AAPL, through.minusYears(13), through),
          Action.RefreshEvaluationPrices(InvestmentEvaluator.Benchmark, through.minusYears(13), through)
        )
      }
    }
    "read evaluations without creating plans, jobs or reports" in {
      val createdAt = Instant.parse("2026-09-01T00:00:00Z")
      val run       = InvestmentScreenRun(
        InvestmentScreenId("read-only"),
        createdAt,
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        Nil,
        Nil,
        List(InvestmentCohort("investment", List(AAPL)))
      )
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(evaluations.findCapturePlan(run.id, "2026-10")).thenReturnNone
      when(prices.cached(any[Ticker], any[LocalDate], any[LocalDate])).thenReturnNone
      when(jobs.find(any[String])).thenReturnNone
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          evaluations,
          prices,
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = jobs
        )
        report <- service.evaluate(run.id)
      yield report
      result.asserting { report =>
        report.settled mustBe Some(false)
        report.pendingDataJobs mustBe Nil
        verify(evaluations, never).getOrCreateCapturePlan(any[EvaluationCapturePlan])
        verify(evaluations, never).saveMonthly(any[InvestmentEvaluation])
        verify(jobs, never).enqueue(any[Job])
        verify(jobs, never).retryTerminal(any[String], any[Instant])
        verify(prices, never).history(any[Ticker], any[LocalDate], any[LocalDate])
        succeed
      }
    }
    "settle unaudited and retry-exhausted captures once without claiming complete investment data" in {
      def check(exhausted: Boolean): IO[org.scalatest.Assertion] = {
        val createdAt = Instant.parse("2026-09-01T00:00:00Z")
        val from      = LocalDate.parse("2026-09-01")
        val through   = LocalDate.parse("2026-09-30")
        val run       = InvestmentScreenRun(
          InvestmentScreenId(s"settled-$exhausted"),
          createdAt,
          InvestmentRecipe(),
          InvestmentScreenRequest(),
          Nil,
          Nil,
          List(InvestmentCohort("investment", List(AAPL)))
        )
        val plan        = EvaluationCapturePlan(run.id, "2026-10", through, now)
        val evaluations = mock[InvestmentEvaluationRepository[IO]]
        val prices      = mock[EvaluationPriceHistory[IO]]
        val jobs        = mock[JobRepository[IO]]
        when(evaluations.capturePlans).thenReturnIO(Nil)
        when(evaluations.unsettled).thenReturnIO(Nil)
        when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
        when(evaluations.getOrCreateCapturePlan(any[EvaluationCapturePlan])).thenReturnIO(plan)
        if exhausted then {
          when(prices.cached(any[Ticker], any[LocalDate], any[LocalDate])).thenReturnNone
          when(jobs.find(any[String]))
            .thenReturnSome(Job("failed", Action.RefreshEvaluationPrices(AAPL, from, through), now, now, state = Job.Failed))
          when(jobs.retryTerminal(any[String], any[Instant])).thenReturnIO(false)
        } else {
          val series = EvaluationPriceSeries(
            AAPL,
            List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 105)),
            "unaudited",
            now,
            false,
            "split-and-dividend",
            DateRange(from, through)
          )
          when(prices.cached(any[Ticker], any[LocalDate], any[LocalDate])).thenAnswer(invocation =>
            IO.pure(Some(series.copy(ticker = invocation.getArgument[Ticker](0))))
          )
        }
        for
          saved <- Ref.of[IO, Option[InvestmentEvaluation]](None)
          _ = when(evaluations.findMonthly(run.id, "2026-10")).thenReturn(saved.get)
          _ = when(evaluations.saveMonthly(any[InvestmentEvaluation])).thenAnswer(invocation =>
            saved.set(Some(invocation.getArgument[InvestmentEvaluation](0)))
          )
          state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
          service <- InvestmentScreenService.make(
            mock[StockRepository[IO]],
            mock[FinancialHistoryRepository[IO]],
            memoryScreens(state),
            evaluations,
            prices,
            investmentConfig(),
            rankings = mock[InvestmentRankingRepository[IO]],
            jobs = jobs
          )
          first  <- service.captureMonthlyEvaluations
          second <- service.captureMonthlyEvaluations
          report <- service.evaluate(run.id)
        yield
          first mustBe 1
          second mustBe 0
          report.settled mustBe Some(true)
          report.pendingDataJobs mustBe Nil
          InvestmentEvaluationRepository.complete(report) mustBe false
          verify(evaluations).saveMonthly(any[InvestmentEvaluation])
          verify(prices, times(2)).cached(any[Ticker], any[LocalDate], any[LocalDate])
          if exhausted then {
            report.horizons.flatMap(_.issues).mkString must include("repair attempts exhausted")
            verify(jobs, times(2)).retryTerminal(any[String], any[Instant])
          } else verifyNoInteractions(jobs)
          succeed
      }
      (check(false) >> check(true)).asserting(identity)
    }
    "stop creating monthly work after the last horizon settles and return that final capture" in {
      val createdAt = Instant.parse("2020-01-01T00:00:00Z")
      val through   = LocalDate.parse("2025-01-15")
      val run       = InvestmentScreenRun(
        InvestmentScreenId("finished"),
        createdAt,
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        Nil,
        Nil,
        List(InvestmentCohort("investment", List(AAPL)))
      )
      val finalReport = InvestmentEvaluator
        .evaluate(run.id, createdAt, now, run.cohorts, Map.empty, Some(through))
        .copy(captureMonth = Some("2025-02"), settled = Some(true))
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(evaluations.capturePlans).thenReturnIO(Nil)
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set(run.id))
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(evaluations.findFinished(run.id)).thenReturnSome(finalReport)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          evaluations,
          prices,
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = jobs
        )
        count  <- service.captureMonthlyEvaluations
        report <- service.evaluate(run.id)
      yield (count, report)
      result.asserting { case (count, report) =>
        count mustBe 0
        report mustBe finalReport
        verify(evaluations, never).getOrCreateCapturePlan(any[EvaluationCapturePlan])
        verify(evaluations, never).saveMonthly(any[InvestmentEvaluation])
        verifyNoInteractions(prices, jobs)
        succeed
      }
    }
    "only read ranking headers when the daily ranking and monthly screen already exist" in {
      val ranking  = InvestmentRankingRepository.Header("published", now, InvestmentRecipe(), InvestmentRankingSummary(100, 25, Map.empty))
      val rankings = mock[InvestmentRankingRepository[IO]]
      val screens  = mock[InvestmentScreenRepository[IO]]
      val id       = InvestmentScreenId("monthly:2026-10:quality-fair-value-v1")
      val header   = InvestmentScreenRepository.Header(
        id,
        now,
        ranking._id,
        InvestmentScreenRequest(),
        Nil,
        InvestmentScreenSummary(id, now, ranking.recipe.version, 100, 25, 20, Map.empty)
      )
      when(rankings.latestHeader).thenReturnSome(ranking)
      when(screens.header(id)).thenReturnSome(header)
      val result = for
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          screens,
          mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]],
          investmentConfig(true, true),
          rankings = rankings,
          jobs = mock[JobRepository[IO]]
        )
        created <- service.generateScheduledScreen
      yield created
      result.asserting { created =>
        created mustBe None
        verify(rankings, never).latest
        verify(rankings, never).find(any[String])
        verify(screens, never).find(any[InvestmentScreenId])
        succeed
      }
    }
    "show one failed intrinsic financial criterion without pretending it has an investment score" in {
      val annual = (1 to 5).toList.map(n =>
        FinancialObservation(
          "fcfMargin",
          LocalDate.of(2026 - n, 12, 31),
          if n <= 2 then BigDecimal("-0.01") else BigDecimal("0.05"),
          "ratio"
        )
      )
      val financial = FinancialSnapshot("facts", AAPL, "Finnhub", now, Map.empty, Nil, Nil, "", "hash", "mapping", Nil)
      val evidence  = InvestmentEvidence(
        "Finnhub",
        now,
        "mapping",
        "hash",
        Map("fcfMargin" -> annual.map(_.period)),
        annual,
        None,
        None,
        Some("USD")
      )
      val card = InvestmentScorecard(
        AAPL,
        false,
        None,
        Map.empty,
        Map.empty,
        List("insufficient-cash-flow-durability"),
        Nil,
        None,
        20,
        Some("facts"),
        "quality-fair-value-v1",
        evidence = Some(evidence)
      )
      val run = InvestmentScreenRun(
        InvestmentScreenId("intrinsic"),
        now,
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        List(InvestmentCandidate(AAPLStock, Some(financial))),
        List(card),
        Nil
      )
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentScreenService.make(
          mock[StockRepository[IO]],
          mock[FinancialHistoryRepository[IO]],
          memoryScreens(state),
          mock[InvestmentEvaluationRepository[IO]],
          mock[EvaluationPriceHistory[IO]],
          investmentConfig(),
          rankings = mock[InvestmentRankingRepository[IO]],
          jobs = mock[JobRepository[IO]]
        )
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
