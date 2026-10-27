package stockschecker.actions

import stockschecker.domain.Provider
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.{Job, JobId, JobStatusSummary}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*

import scala.concurrent.duration.*

class JobExecutionPolicySpec extends AnyWordSpec with Matchers:
  import JobExecutionPolicy.*

  "Job execution policy" should {
    "increase parent polling delays up to five minutes independently of execution retries" in {
      val job = Job(JobId("parent"), Action.SetSecurityActive(AAPL, false, java.time.Instant.EPOCH), ts, ts, retries = MaxTransientRetries)
      pollDelaySeconds(job) mustBe 1L
      List(0 -> 1L, 1 -> 2L, 8 -> 256L, 9 -> 300L, 30 -> 300L).foreach { case (waitCount, expected) =>
        pollDelaySeconds(job.copy(waitCount = waitCount)) mustBe expected
      }
    }

    "wait for missing dependencies while permitting partially successful preceding steps" in {
      dependencies(2, JobStatusSummary(Map(Job.Succeeded -> 1L))) mustBe Progress.Wait
      dependencies(2, JobStatusSummary(Map(Job.Succeeded -> 1L, Job.Partial -> 1L))) mustBe Progress.Ready
      dependencies(2, JobStatusSummary(Map(Job.Failed -> 1L))) mustBe
        Progress.Finish(Job.Cancelled, Some("A preceding step did not succeed"))
    }

    "retain partial child results and wait for unfinished or missing children" in {
      children(2, JobStatusSummary(Map(Job.Succeeded -> 1L))) mustBe Progress.Wait
      children(2, JobStatusSummary(Map(Job.Succeeded -> 1L, Job.Failed -> 1L))) mustBe
        Progress.Finish(Job.Partial, Some("One or more child jobs failed; successful results were retained"))
      children(1, JobStatusSummary(Map(Job.Cancelled -> 1L))) mustBe
        Progress.Finish(Job.Cancelled, Some("One or more child jobs were cancelled"))
    }

    "distinguish provider deferral and configuration from bounded transient failures" in {
      failure(AppError.ProviderDeferred(Provider.Finnhub, ts, "cooldown"), MaxTransientRetries) mustBe Failure.Defer(ts)
      failure(AppError.ProviderConfiguration(Provider.Finnhub, "key"), 0) mustBe Failure.Pause(Provider.Finnhub)
      failure(AppError.DataUnavailable(AAPL, "missing"), MaxTransientRetries - 1) mustBe Failure.Retry
      failure(AppError.DataUnavailable(AAPL, "missing"), MaxTransientRetries) mustBe Failure.Fail
      failure(AppError.HttpClient(Provider.Finnhub.print, 401, "unauthorized"), 0) mustBe Failure.Fail
      failure(AppError.HttpClient(Provider.Finnhub.print, 503, "unavailable"), 0) mustBe Failure.Retry
      retryDelay(0, MinimumRetryJitterMillis) mustBe 1500.millis
      retryDelay(2, MaximumRetryJitterMillis) mustBe 5500.millis
    }

    "assign evaluation backfills lower execution priority than ordinary work" in {
      ActionPriority.forAction(Action.SetSecurityActive(AAPL, false, ts)) mustBe ActionPriority.Internal
      ActionPriority.forAction(
        Action.RefreshEvaluationPrices(
          AAPL,
          ts.atZone(java.time.ZoneOffset.UTC).toLocalDate
        )
      ) mustBe ActionPriority.Evaluation
      ActionPriority.Normal mustBe 0
      ActionPriority.Evaluation mustBe 1
    }
  }
