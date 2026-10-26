package stockschecker.services

import cats.effect.{IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.Action
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.*

import java.time.{Instant, LocalDate}

class InvestmentEvaluationServiceSpec extends IOWordSpec:
  private val now = Instant.parse("2026-10-19T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  private def memoryScreens(state: Ref[IO, List[InvestmentScreenRun]]): InvestmentScreenRepository[IO] =
    new InvestmentScreenRepository[IO]:
      def save(run: InvestmentScreenRun): IO[Unit]              = state.update(_ :+ run)
      def find(id: InvestmentScreenId): IO[InvestmentScreenRun] =
        state.get.flatMap(runs => IO.fromOption(runs.find(_.id == id))(AppError.EntityDoesNotExist("Investment screen", id.value)))
      def header(id: InvestmentScreenId): IO[Option[InvestmentScreenRepository.Header]] = state.get
        .map(_.find(_.id == id))
        .map(
          _.map(r =>
            InvestmentScreenRepository.Header(r.id, r.createdAt, r.rankingId, r.request, r.cohorts, InvestmentScreenRepository.summary(r))
          )
        )
      def list(limit: Int): IO[List[InvestmentScreenSummary]] = state.get.map(_.reverse.take(limit).map(InvestmentScreenRepository.summary))
      def ids: IO[List[InvestmentScreenId]]                   = state.get.map(_.map(_.id))

  "InvestmentEvaluationService" should {
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
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
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
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      val plan        = EvaluationCapturePlan(run.id, "2026-10", through, now)
      when(evaluations.capturePlans).thenReturnIO(List(plan))
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
      when(evaluations.getOrCreateCapturePlan(any[EvaluationCapturePlan])).thenReturnIO(plan)
      when(evaluations.findCapturePlan(run.id, "2026-10")).thenReturnSome(plan)
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(prices.cached(any[Ticker], any[DateRange])).thenReturnNone
      when(jobs.find(any[JobId]))
        .thenReturnSome(Job(JobId("failed"), Action.RefreshEvaluationPrices(AAPL, from, through), now, now, state = Job.Failed))
      when(jobs.retryTerminal(any[JobId], any[Instant])).thenReturnIO(true)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
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
      when(prices.cached(AAPL, DateRange(from, through))).thenReturnSome(series)
      when(prices.cached(InvestmentEvaluator.Benchmark, DateRange(from, through)))
        .thenReturnSome(series.copy(ticker = InvestmentEvaluator.Benchmark))
      val result = for
        service <- InvestmentEvaluationService.make(
          screens,
          evaluations,
          prices,
          jobs
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
      when(prices.cached(any[Ticker], any[DateRange])).thenReturnNone
      when(jobs.find(any[JobId])).thenReturnNone
      when(jobs.enqueue(any[Job])).thenReturnUnit
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](screens)
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
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
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(evaluations.findCapturePlan(run.id, "2026-10")).thenReturnNone
      when(prices.cached(any[Ticker], any[DateRange])).thenReturnNone
      when(jobs.find(any[JobId])).thenReturnNone
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
          prices,
          jobs
        )
        report <- service.evaluate(run.id)
      yield report
      result.asserting { report =>
        report.settled mustBe Some(false)
        report.pendingDataJobs mustBe Nil
        verify(evaluations, never).getOrCreateCapturePlan(any[EvaluationCapturePlan])
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
        val prices      = mock[EvaluationPriceHistory[IO]]
        val jobs        = mock[JobRepository[IO]]
        when(evaluations.capturePlans).thenReturnIO(Nil)
        when(evaluations.unsettled).thenReturnIO(Nil)
        when(evaluations.finishedScreenIds).thenReturnIO(Set.empty[InvestmentScreenId])
        when(evaluations.getOrCreateCapturePlan(any[EvaluationCapturePlan])).thenReturnIO(plan)
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
          state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
          service <- InvestmentEvaluationService.make(
            memoryScreens(state),
            evaluations,
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
          InvestmentEvaluationRepository.complete(report) mustBe false
          verify(evaluations).saveMonthly(any[InvestmentEvaluation])
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
      val prices      = mock[EvaluationPriceHistory[IO]]
      val jobs        = mock[JobRepository[IO]]
      when(evaluations.capturePlans).thenReturnIO(Nil)
      when(evaluations.unsettled).thenReturnIO(Nil)
      when(evaluations.finishedScreenIds).thenReturnIO(Set(run.id))
      when(evaluations.findMonthly(run.id, "2026-10")).thenReturnNone
      when(evaluations.findFinished(run.id)).thenReturnSome(finalReport)
      val result = for
        state   <- Ref.of[IO, List[InvestmentScreenRun]](List(run))
        service <- InvestmentEvaluationService.make(
          memoryScreens(state),
          evaluations,
          prices,
          jobs
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
  }
