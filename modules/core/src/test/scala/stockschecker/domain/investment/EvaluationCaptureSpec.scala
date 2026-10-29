package stockschecker.domain.investment

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.*
import stockschecker.fixtures.AAPL
import stockschecker.repositories.InvestmentRepositoryFixtures.*

import java.time.{Instant, LocalDate, YearMonth}

class EvaluationCaptureSpec extends AnyWordSpec with Matchers:
  private val month   = YearMonth.of(2026, 10)
  private val current = screen.copy(createdAt = Instant.parse("2026-09-01T00:00:00Z"))

  "EvaluationCapture" should {
    "capture only completed months and include each cohort ticker and benchmark once" in {
      val duplicated = current.copy(cohorts =
        List(
          InvestmentCohort(CohortName.Investment, List(AAPL, AAPL, InvestmentEvaluator.Benchmark)),
          InvestmentCohort(CohortName.Investment, List(AAPL))
        )
      )
      EvaluationCapture.hasCapture(current, YearMonth.of(2026, 9)) mustBe false
      EvaluationCapture.hasCapture(current, month) mustBe true
      EvaluationCapture.range(current, month) mustBe DateRange(LocalDate.parse("2026-09-01"), LocalDate.parse("2026-09-30"))
      EvaluationCapture.tickers(duplicated) mustBe List(InvestmentEvaluator.Benchmark, AAPL)
    }

    "record unresolved histories only when jobs have finished and keep pending IDs stable" in {
      val draft   = EvaluationCapture.draft(current, now, month, Map.empty)
      val jobs    = Set(JobId("z"), JobId("a"))
      val pending = draft.settle(jobs)
      val settled = draft.settle(Set.empty)
      draft.needed mustBe List(InvestmentEvaluator.Benchmark, AAPL)
      pending.settled mustBe false
      pending.pendingDataJobs mustBe List(JobId("a"), JobId("z"))
      pending.horizons.flatMap(_.issues).mkString must not include "after completed capture jobs"
      settled.settled mustBe true
      settled.horizons.foreach(_.issues.mkString must include("Price history remains unavailable after completed capture jobs"))
    }

    "prefer a settled monthly capture and use a finished capture only when the monthly capture is absent and the horizon is past" in {
      val old      = current.copy(createdAt = Instant.parse("2020-01-01T00:00:00Z"))
      val monthly  = EvaluationCapture.draft(old, now, month, Map.empty).report
      val finished = monthly.copy(captureMonth = YearMonth.of(2025, 2), settled = true)
      EvaluationCapture.Context(old, None, Some(finished)).settledReport(now) mustBe Some(finished)
      EvaluationCapture.Context(old, Some(monthly), Some(finished)).settledReport(now) mustBe None
      EvaluationCapture
        .Context(old, Some(monthly.copy(settled = true)), Some(finished))
        .settledReport(now) mustBe Some(monthly.copy(settled = true))
      EvaluationCapture.Context(current, None, Some(finished)).settledReport(now) mustBe None
    }
  }
