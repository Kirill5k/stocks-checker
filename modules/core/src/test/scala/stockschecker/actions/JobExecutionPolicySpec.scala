package stockschecker.actions

import stockschecker.domain.{FinancialMetric, Provider, ProviderDeferralReason}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.{Job, JobId, JobStatusSummary}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import io.circe.Json
import mongo4cats.errors.MongoJsonParsingException

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

    "back off provider deferrals from thirty seconds to five minutes independently of transient retries" in {
      val job = Job(JobId("deferred"), Action.SetSecurityActive(AAPL, false, ts), ts, ts, retries = MaxTransientRetries)
      List(-1 -> 30L, 0 -> 30L, 1 -> 60L, 2 -> 120L, 3 -> 240L, 4 -> 300L, 30 -> 300L, Int.MaxValue -> 300L).foreach {
        case (waitCount, seconds) =>
          providerDeferUntil(job.copy(waitCount = waitCount), ts, ts.plusSeconds(1), 0) mustBe ts.plusSeconds(seconds)
      }
    }

    "respect provider cooldowns and spread retries after both backoff and daily quota resets" in {
      val job   = Job(JobId("deferred"), Action.SetSecurityActive(AAPL, false, ts), ts, ts)
      val reset = ts.plusSeconds(86400)
      providerDeferUntil(job, ts, reset, 0) mustBe reset
      providerDeferUntil(job, ts, reset, MaximumProviderDeferJitterMillis) mustBe reset.plusSeconds(30)
      providerDeferUntil(job, ts, ts.minusSeconds(1), 0) mustBe ts.plusSeconds(30)
      providerDeferUntil(job, ts, ts.plusSeconds(1), MaximumProviderDeferJitterMillis) mustBe ts.plusSeconds(60)
      providerDeferUntil(job.copy(waitCount = 10), ts, ts.plusSeconds(1), MaximumProviderDeferJitterMillis) mustBe ts.plusSeconds(330)
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
      failure(AppError.ProviderDeferred(Provider.Finnhub, ts, ProviderDeferralReason.Provider429), MaxTransientRetries) mustBe Failure
        .Defer(ts)
      failure(AppError.ProviderConfiguration(Provider.Finnhub, "key"), 0) mustBe Failure.Pause(Provider.Finnhub)
      failure(AppError.DataUnavailable(AAPL, "missing"), MaxTransientRetries - 1) mustBe Failure.Retry
      failure(AppError.DataUnavailable(AAPL, "missing"), MaxTransientRetries) mustBe Failure.Fail
      failure(AppError.HttpClient(Provider.Finnhub.print, 401, "unauthorized"), 0) mustBe Failure.Fail
      failure(AppError.HttpClient(Provider.Finnhub.print, 503, "unavailable"), 0) mustBe Failure.Retry
      retryDelay(0, MinimumRetryJitterMillis) mustBe 1500.millis
      retryDelay(2, MaximumRetryJitterMillis) mustBe 5500.millis
    }

    "report Mongo decoding field paths without exposing stored documents or invalid values" in {
      val document = Json.obj("job" -> Json.obj("waitCount" -> Json.Null, "privateValue" -> Json.fromString("secret-document")))
      val decoding = document.hcursor.downField("job").get[Int]("waitCount").swap.toOption.get
      val error    = MongoJsonParsingException(decoding.getMessage, Some(document.noSpaces))
      errorSummary(error) must include("$.job.waitCount")
      errorSummary(error) must not include "secret-document"
      errorSummary(error) must not include "privateValue"

      val invalidValue = MongoJsonParsingException(
        "DecodingFailure at .financialSnapshots[0].snapshot.metrics: Invalid value secret-response",
        Some("secret-document")
      )
      errorSummary(invalidValue) mustBe "MongoJsonParsingException at $.financialSnapshots[0].snapshot.metrics (invalid value)"
      errorSummary(MongoJsonParsingException("DecodingFailure at .evaluation.dataThrough: Missing required field")) mustBe
        "MongoJsonParsingException at $.evaluation.dataThrough (missing required field)"
      errorSummary(MongoJsonParsingException("DecodingFailure at .job.waitCount: Int")) mustBe
        "MongoJsonParsingException at $.job.waitCount (expected Int)"
    }

    "retain digit-leading metric field paths from real map decoding failures" in {
      val document = Json.obj(
        "financialSnapshots" -> Json.arr(Json.obj("metrics" -> Json.obj("26WeekPriceReturnDaily" -> Json.fromString("private-value"))))
      )
      val decoding = document.hcursor
        .downField("financialSnapshots")
        .downArray
        .get[Map[FinancialMetric, BigDecimal]]("metrics")
        .swap
        .toOption
        .get
      val error = MongoJsonParsingException(decoding.getMessage, Some(document.noSpaces))
      errorSummary(error) mustBe "MongoJsonParsingException at $.financialSnapshots[0].metrics.26WeekPriceReturnDaily (invalid map key)"
      errorSummary(error) must not include "private-value"
      failure(error, 0) mustBe Failure.Fail
    }

    "omit unrecognized or unsafe Mongo decoding messages and avoid retrying malformed data" in {
      List(
        "Cannot map secret-document bson value to json",
        "DecodingFailure at .field\ninjected: secret-value",
        s"DecodingFailure at .${"x" * 300}: secret-value"
      ).foreach { message =>
        val error = MongoJsonParsingException(message, Some("secret-document"))
        errorSummary(error) mustBe "MongoJsonParsingException (invalid stored data; field path unavailable)"
        failure(error, 0) mustBe Failure.Fail
      }
      failure(new com.mongodb.MongoException("connection failed"), 0) mustBe Failure.Retry
      errorSummary(new java.io.IOException("connection interrupted")) mustBe "connection interrupted"
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
