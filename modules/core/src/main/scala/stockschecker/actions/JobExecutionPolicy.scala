package stockschecker.actions

import stockschecker.domain.Provider
import stockschecker.domain.{Job, JobState, JobStatusSummary}
import stockschecker.domain.errors.AppError

import java.time.Instant
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*

private[actions] object JobExecutionPolicy:
  val MaxTransientRetries              = 3
  val Lease                            = 2.minutes
  val HeartbeatInterval                = 20.seconds
  val IdlePollInterval                 = 250.millis
  val WorkerRecoveryDelay              = 1.second
  val RetentionInterval                = 1.hour
  val MinimumRetryJitterMillis         = 500L
  val MaximumRetryJitterMillis         = 1500L
  val InitialProviderDeferDelay        = 30.seconds
  val MaximumProviderDeferDelay        = 5.minutes
  val MaximumProviderDeferJitterMillis = 30.seconds.toMillis

  enum Progress:
    case Ready
    case Wait
    case Finish(state: JobState, error: Option[String] = None)

  enum Failure:
    case Defer(until: Instant)
    case Pause(provider: Provider)
    case Retry
    case Fail

  def dependencies(required: Int, summary: JobStatusSummary): Progress =
    if summary.count(Job.Failed) + summary.count(Job.Cancelled) > 0 then
      Progress.Finish(Job.Cancelled, Some("A preceding step did not succeed"))
    else if summary.usable < required then Progress.Wait
    else Progress.Ready

  def children(required: Int, summary: JobStatusSummary): Progress =
    if summary.terminal < required then Progress.Wait
    else if summary.count(Job.Failed) + summary.count(Job.Partial) > 0 then
      Progress.Finish(
        if summary.usable > 0 then Job.Partial else Job.Failed,
        Some("One or more child jobs failed; successful results were retained")
      )
    else if summary.count(Job.Cancelled) > 0 then Progress.Finish(Job.Cancelled, Some("One or more child jobs were cancelled"))
    else Progress.Finish(Job.Succeeded)

  def failure(error: Throwable, retries: Int): Failure = error match
    case AppError.ProviderDeferred(_, retryAt, _)               => Failure.Defer(retryAt)
    case AppError.ProviderConfiguration(provider, _)            => Failure.Pause(provider)
    case _ if transient(error) && retries < MaxTransientRetries => Failure.Retry
    case _                                                      => Failure.Fail

  def retryDelay(retries: Int, jitterMillis: Long): FiniteDuration = ((1000L << retries) + jitterMillis).millis

  def providerDeferUntil(job: Job, now: Instant, retryAt: Instant, jitterMillis: Long): Instant =
    val delayMillis  = (InitialProviderDeferDelay.toMillis << job.waitCount.max(0).min(30)).min(MaximumProviderDeferDelay.toMillis)
    val backoffUntil = now.plusMillis(delayMillis)
    val earliest     = if retryAt.isAfter(backoffUntil) then retryAt else backoffUntil
    earliest.plusMillis(jitterMillis)

  def pollDelaySeconds(job: Job): Long = (1L << job.waitCount.min(9)).min(300L)

  private def transient(error: Throwable): Boolean = error match
    case _: AppError.DataUnavailable       => true
    case AppError.HttpClient(_, status, _) => status == 408 || status == 429 || status >= 500
    case _: java.io.IOException | _: TimeoutException | _: com.mongodb.MongoException | _: sttp.client4.SttpClientException => true
    case _                                                                                                                  => false
