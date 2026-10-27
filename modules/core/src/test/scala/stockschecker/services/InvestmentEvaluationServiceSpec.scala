package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.Action
import stockschecker.domain.*
import stockschecker.utils.InvestmentEvaluator
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.*

import java.time.{Instant, LocalDate}

class InvestmentEvaluationServiceSpec extends IOWordSpec:
  private val now = Instant.parse("2026-10-19T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  private def memoryScreens(state: Ref[IO, List[InvestmentScreenHeader]]): InvestmentScreenRepository[IO] =
    new InvestmentScreenRepository[IO]:
      def save(header: InvestmentScreenHeader): IO[Unit]           = state.update(_ :+ header)
      def find(id: InvestmentScreenId): IO[InvestmentScreenHeader] =
        header(id).flatMap(value => IO.fromOption(value)(AppError.EntityDoesNotExist("Investment screen", id.value)))
      def header(id: InvestmentScreenId): IO[Option[InvestmentScreenHeader]] = state.get.map(_.find(_.id == id))
      def list(limit: Int): IO[List[InvestmentScreenSummary]]                = state.get.map(_.reverse.take(limit).map(_.summary))
      def ids: IO[List[InvestmentScreenId]]                                  = state.get.map(_.map(_.id))
      def rankingIds: IO[Set[InvestmentRankingId]]                           = state.get.map(_.map(_.rankingId).toSet)

  "InvestmentEvaluationService" should {
    "return pending evaluation for a new saved cohort without fetching nonexistent future returns" in {
      val prices      = mock[EvaluationPriceHistory[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
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
        state   <- Ref.of[IO, List[InvestmentScreenHeader]](List(InvestmentScreenHeader.from(run)))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
          plans,
          prices,
          mock[JobRepository[IO]]
        )
        evaluated <- service.evaluate(run.id)
      yield evaluated
      result.asserting { report =>
        report.horizons.map(_.status) mustBe List("pending", "pending")
        verifyNoInteractions(prices)
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
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      val plan        = EvaluationCapturePlan(run.id, "2026-10", through, now)
      when(plans.all).thenReturnIO(List(plan))
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(plans.getOrCreate(any[EvaluationCapturePlan])).thenReturnIO(plan)
      when(plans.find(run.id, "2026-10")).thenReturnSome(plan)
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(prices.cached(any[Ticker], any[DateRange])).thenReturnNone
      when(jobs.find(any[JobId]))
        .thenReturnSome(Job(JobId("failed"), Action.RefreshEvaluationPrices(AAPL, from, through), now, now, state = Job.Failed))
      when(jobs.retryTerminal(any[JobId], any[Instant])).thenReturnIO(true)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenHeader]](List(InvestmentScreenHeader.from(run)))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
          plans,
          prices,
          jobs
        )
        report <- service.evaluate(run.id)
        saved  <- service.captureMonthlyEvaluations
      yield (report, saved)
      result.asserting { case (report, saved) =>
        report.pendingDataJobs mustBe Nil
        report.settled mustBe Some(false)
        report.dataThrough mustBe Some(through)
        saved mustBe 0
        verify(jobs, times(2)).retryTerminal(any[JobId], any[Instant])
        verify(evaluations, never).saveMonthly(any[InvestmentEvaluation])
        verify(prices, never).history(any[Ticker], any[DateRange])
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
        InvestmentScreenHeader(
          run.id,
          createdAt,
          InvestmentRankingId("ranking"),
          run.request,
          run.cohorts,
          InvestmentScreenSummary.from(run)
        )
      val screens     = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
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
      when(screens.find(run.id)).thenReturnIO(header)
      when(plans.all).thenReturnIO(Nil)
      when(plans.getOrCreate(any[EvaluationCapturePlan])).thenReturnIO(oldPlan)
      when(evaluations.unsettled).thenReturnIO(List(incomplete))
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(evaluations.findMonthly(run.id, "2026-09")).thenReturnSome(incomplete)
      when(evaluations.saveMonthly(any[InvestmentEvaluation])).thenReturnUnit
      when(plans.retire(run.id, "2026-09")).thenReturnUnit
      when(prices.cached(AAPL, DateRange(from, through))).thenReturnSome(series)
      when(prices.cached(InvestmentEvaluator.Benchmark, DateRange(from, through)))
        .thenReturnSome(series.copy(ticker = InvestmentEvaluator.Benchmark))
      val result = for
        service <- InvestmentEvaluationService.make(
          screens,
          evaluations,
          plans,
          prices,
          jobs
        )
        saved <- service.captureMonthlyEvaluations
      yield saved
      result.asserting { saved =>
        val captor = org.mockito.ArgumentCaptor.forClass(classOf[InvestmentEvaluation])
        verify(evaluations).saveMonthly(captor.capture())
        val planCaptor = org.mockito.ArgumentCaptor.forClass(classOf[EvaluationCapturePlan])
        verify(plans).getOrCreate(planCaptor.capture())
        planCaptor.getValue.month mustBe "2026-09"
        planCaptor.getValue.through mustBe through
        captor.getValue.captureMonth mustBe Some("2026-09")
        captor.getValue.dataThrough mustBe Some(through)
        captor.getValue.isComplete mustBe true
        saved mustBe 1
        verify(plans).retire(run.id, "2026-09")
        verify(screens).find(run.id)
        verifyNoInteractions(jobs)
        succeed
      }
    }
    "retain a capture plan when persisting its settled evaluation fails" in {
      val from    = LocalDate.parse("2026-09-01")
      val through = LocalDate.parse("2026-09-30")
      val run     = InvestmentScreenRun(
        InvestmentScreenId("failed-save"),
        Instant.parse("2026-09-01T00:00:00Z"),
        InvestmentRecipe(),
        InvestmentScreenRequest(),
        Nil,
        Nil,
        List(InvestmentCohort("investment", List(AAPL)))
      )
      val plan        = EvaluationCapturePlan(run.id, "2026-10", through, now)
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      val failure     = new IllegalStateException("evaluation write failed")
      val series      = EvaluationPriceSeries(
        AAPL,
        List(EvaluationClose(from.plusDays(1), 100), EvaluationClose(through, 105)),
        "unaudited",
        now,
        false,
        "split-and-dividend",
        DateRange(from, through)
      )
      when(plans.all).thenReturnIO(List(plan))
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(evaluations.findMonthly(run.id, plan.month)).thenReturnNone
      when(evaluations.saveMonthly(any[InvestmentEvaluation])).thenReturn(IO.raiseError(failure))
      when(prices.cached(any[Ticker], any[DateRange])).thenAnswer(invocation =>
        IO.pure(Some(series.copy(ticker = invocation.getArgument[Ticker](0))))
      )
      val result = for
        state    <- Ref.of[IO, List[InvestmentScreenHeader]](List(InvestmentScreenHeader.from(run)))
        service  <- InvestmentEvaluationService.make(memoryScreens(state), evaluations, plans, prices, jobs)
        captured <- service.captureMonthlyEvaluations.attempt
      yield captured
      result.asserting { captured =>
        captured mustBe Left(failure)
        verify(evaluations).saveMonthly(any[InvestmentEvaluation])
        verify(plans, never).retire(any[InvestmentScreenId], any[String])
        verifyNoInteractions(jobs)
        succeed
      }
    }
    "retire leftover plans for settled reports without rebuilding or saving the report" in {
      val id     = InvestmentScreenId("cleanup-recovery")
      val plan   = EvaluationCapturePlan(id, "2026-09", LocalDate.parse("2026-08-31"), now)
      val report = InvestmentEvaluator
        .evaluate(id, Instant.parse("2026-08-01T00:00:00Z"), now, Nil, Map.empty, Some(plan.through))
        .copy(captureMonth = Some(plan.month), settled = Some(true))
      val screens     = mock[InvestmentScreenRepository[IO]]
      val evaluations = mock[InvestmentEvaluationRepository[IO]]
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(screens.ids).thenReturnIO(Nil)
      when(plans.all).thenReturnIO(List(plan))
      when(plans.retire(id, plan.month)).thenReturnUnit
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(evaluations.findMonthly(id, plan.month)).thenReturnSome(report)
      val result = for
        service <- InvestmentEvaluationService.make(screens, evaluations, plans, prices, jobs)
        count   <- service.captureMonthlyEvaluations
      yield count
      result.asserting { count =>
        count mustBe 0
        verify(plans).retire(id, plan.month)
        verify(evaluations, never).saveMonthly(any[InvestmentEvaluation])
        verify(screens, never).find(any[InvestmentScreenId])
        verifyNoInteractions(prices, jobs)
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
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(evaluations.findMonthly(any[InvestmentScreenId], any[String])).thenReturnNone
      when(plans.all).thenReturnIO(Nil)
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(plans.getOrCreate(any[EvaluationCapturePlan])).thenAnswer(invocation =>
        IO.pure(invocation.getArgument[EvaluationCapturePlan](0))
      )
      when(prices.cached(any[Ticker], any[DateRange])).thenReturnNone
      when(jobs.find(any[JobId])).thenReturnNone
      when(jobs.enqueue(any[Job])).thenReturnUnit
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenHeader]](screens.map(InvestmentScreenHeader.from))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
          plans,
          prices,
          jobs
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
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(plans.find(run.id, "2026-10")).thenReturnNone
      when(prices.cached(any[Ticker], any[DateRange])).thenReturnNone
      when(jobs.find(any[JobId])).thenReturnNone
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenHeader]](List(InvestmentScreenHeader.from(run)))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
          plans,
          prices,
          jobs
        )
        report <- service.evaluate(run.id)
      yield report
      result.asserting { report =>
        report.settled mustBe Some(false)
        report.pendingDataJobs mustBe Nil
        verify(plans, never).getOrCreate(any[EvaluationCapturePlan])
        verify(plans, never).retire(any[InvestmentScreenId], any[String])
        verify(evaluations, never).saveMonthly(any[InvestmentEvaluation])
        verify(jobs, never).enqueue(any[Job])
        verify(jobs, never).retryTerminal(any[JobId], any[Instant])
        verify(prices, never).history(any[Ticker], any[DateRange])
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
        val plans       = mock[EvaluationCapturePlanRepository[IO]]
        val prices      = mock[EvaluationPriceHistory[IO]]
        val jobs        = mock[JobRepository[IO]]
        when(plans.all).thenReturnIO(Nil)
        when(evaluations.unsettled).thenReturnIO(Nil)
        when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
        when(plans.getOrCreate(any[EvaluationCapturePlan])).thenReturnIO(plan)
        when(plans.retire(run.id, "2026-10")).thenReturnUnit
        if exhausted then {
          when(prices.cached(any[Ticker], any[DateRange])).thenReturnNone
          when(jobs.find(any[JobId]))
            .thenReturnSome(Job(JobId("failed"), Action.RefreshEvaluationPrices(AAPL, from, through), now, now, state = Job.Failed))
          when(jobs.retryTerminal(any[JobId], any[Instant])).thenReturnIO(false)
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
          when(prices.cached(any[Ticker], any[DateRange])).thenAnswer(invocation =>
            IO.pure(Some(series.copy(ticker = invocation.getArgument[Ticker](0))))
          )
        }
        for
          saved <- Ref.of[IO, Option[InvestmentEvaluation]](None)
          _ = when(evaluations.findMonthly(run.id, "2026-10")).thenReturn(saved.get)
          _ = when(evaluations.saveMonthly(any[InvestmentEvaluation])).thenAnswer(invocation =>
            saved.set(Some(invocation.getArgument[InvestmentEvaluation](0)))
          )
          state   <- Ref.of[IO, List[InvestmentScreenHeader]](List(InvestmentScreenHeader.from(run)))
          service <- InvestmentEvaluationService.make(
            memoryScreens(state),
            evaluations,
            plans,
            prices,
            jobs
          )
          first  <- service.captureMonthlyEvaluations
          second <- service.captureMonthlyEvaluations
          report <- service.evaluate(run.id)
        yield
          first mustBe 1
          second mustBe 0
          report.settled mustBe Some(true)
          report.pendingDataJobs mustBe Nil
          report.isComplete mustBe false
          verify(evaluations).saveMonthly(any[InvestmentEvaluation])
          verify(plans).retire(run.id, "2026-10")
          verify(prices, times(2)).cached(any[Ticker], any[DateRange])
          if exhausted then {
            report.horizons.flatMap(_.issues).mkString must include("repair attempts exhausted")
            verify(jobs, times(2)).retryTerminal(any[JobId], any[Instant])
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
      val plans       = mock[EvaluationCapturePlanRepository[IO]]
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(plans.all).thenReturnIO(Nil)
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set(run.id))
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(evaluations.findFinished(run.id)).thenReturnSome(finalReport)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenHeader]](List(InvestmentScreenHeader.from(run)))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
          plans,
          prices,
          jobs
        )
        count  <- service.captureMonthlyEvaluations
        report <- service.evaluate(run.id)
      yield (count, report)
      result.asserting { case (count, report) =>
        count mustBe 0
        report mustBe finalReport
        verify(plans, never).getOrCreate(any[EvaluationCapturePlan])
        verify(plans, never).retire(any[InvestmentScreenId], any[String])
        verify(evaluations, never).saveMonthly(any[InvestmentEvaluation])
        verifyNoInteractions(prices, jobs)
        succeed
      }
    }
  }
