package stockschecker.actions

import cats.effect.{Deferred, IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import mongo4cats.errors.MongoJsonParsingException
import org.mockito.ArgumentCaptor
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.{Job, JobId, JobState, JobStatusSummary, Provider, ProviderDeferralReason}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.JobRepository

import java.time.Instant
import scala.concurrent.duration.*

class ActionExecutorSpec extends IOWordSpec:
  given Clock[IO]  = Clock.mock(ts)
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  final private class RecordingLogger(messages: Ref[IO, List[(String, String)]]) extends Logger[IO]:
    private def record(level: String, message: => String): IO[Unit] =
      IO.delay(message).flatMap(value => messages.update(_ :+ (level -> value)))

    override def debug(message: => String): IO[Unit]                   = record("debug", message)
    override def info(message: => String): IO[Unit]                    = record("info", message)
    override def warn(message: => String): IO[Unit]                    = record("warn", message)
    override def error(message: => String): IO[Unit]                   = record("error", message)
    override def trace(message: => String): IO[Unit]                   = record("trace", message)
    override def debug(error: Throwable)(message: => String): IO[Unit] = record("debug", message)
    override def info(error: Throwable)(message: => String): IO[Unit]  = record("info", message)
    override def warn(error: Throwable)(message: => String): IO[Unit]  = record("warn", message)
    override def error(error: Throwable)(message: => String): IO[Unit] = record("error", message)
    override def trace(error: Throwable)(message: => String): IO[Unit] = record("trace", message)

  private def executeAndRecordLogs(jobs: JobRepository[IO], handler: ActionHandler[IO]): IO[List[(String, String)]] =
    Ref.of[IO, List[(String, String)]](Nil).flatMap { messages =>
      given Logger[IO] = new RecordingLogger(messages)
      for
        executor <- ActionExecutor.make(jobs, handler, _ => IO.pure(true), ActionExecutorConfig(1))
        _        <- executor.run.take(1).compile.drain.timeout(3.seconds)
        logs     <- messages.get
      yield logs
    }

  "An action executor" should {
    "back off repeated provider deferrals without warning or consuming transient retries" in {
      val jobs   = mock[JobRepository[IO]]
      val action = Action.SetSecurityActive(AAPL, false, ts)
      val job    = Job(
        JobId("deferred"),
        action,
        ts,
        ts,
        state = Job.Running,
        retries = JobExecutionPolicy.MaxTransientRetries,
        waitCount = 3,
        leaseToken = Some("owner")
      )
      val error = AppError.ProviderDeferred(
        Provider.TwelveData,
        ts.plusSeconds(1),
        ProviderDeferralReason.ConcurrencyLimit,
        Some("provider context with api_key=secret-value")
      )
      val scheduled = ArgumentCaptor.forClass(classOf[Instant])
      when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturn(IO.pure(Some(job)), IO.pure(None))
      when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
      when(jobs.defer(any[Job], any[Instant], any[Option[String]], any[Boolean])).thenReturnUnit
      when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
      val handler = new ActionHandler[IO] {
        def execute(action: Action): IO[ActionResult] = IO.raiseError(error)
      }
      executeAndRecordLogs(jobs, handler).asserting { logs =>
        verify(jobs).defer(eqTo(job), scheduled.capture(), eqTo(Some(AppError.safeSummary(error))), eqTo(false))
        scheduled.getValue.isBefore(ts.plusSeconds(240)) mustBe false
        scheduled.getValue.isAfter(ts.plusSeconds(270)) mustBe false
        verify(jobs, never).finish(any[Job], any[JobState], any[Option[String]])
        logs mustBe Nil
        succeed
      }
    }

    "continue warning when an actual job failure is terminal" in {
      val jobs   = mock[JobRepository[IO]]
      val action = Action.SetSecurityActive(AAPL, false, ts)
      val job    = Job(JobId("failed"), action, ts, ts, state = Job.Running, leaseToken = Some("owner"))
      val error  = AppError.FailedValidation("Invalid provider data")
      when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturn(IO.pure(Some(job)), IO.pure(None))
      when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
      when(jobs.finish(any[Job], any[JobState], any[Option[String]])).thenReturnUnit
      when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
      val handler = new ActionHandler[IO] {
        def execute(action: Action): IO[ActionResult] = IO.raiseError(error)
      }
      executeAndRecordLogs(jobs, handler).asserting { logs =>
        verify(jobs).finish(job, Job.Failed, Some(AppError.safeSummary(error)))
        verify(jobs, never).defer(any[Job], any[Instant], any[Option[String]], any[Boolean])
        logs mustBe List("warn" -> s"Job ${job.id} failed: ${AppError.safeSummary(error)}")
        succeed
      }
    }

    "persist safe decoding diagnostics without retrying invalid stored data" in {
      val jobs  = mock[JobRepository[IO]]
      val job   = Job(JobId("invalid-data"), Action.RefreshInvestmentResearch, ts, ts, state = Job.Running, leaseToken = Some("owner"))
      val error = MongoJsonParsingException(
        "DecodingFailure at .financialSnapshots[0].metrics: Invalid value api_key=secret-value",
        Some("{\"rawPayload\":\"private-provider-response\",\"api_key\":\"secret-value\"}")
      )
      when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturn(IO.pure(Some(job)), IO.pure(None))
      when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
      when(jobs.finish(any[Job], any[JobState], any[Option[String]])).thenReturnUnit
      when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
      val handler = new ActionHandler[IO] {
        def execute(action: Action): IO[ActionResult] = IO.raiseError(error)
      }
      executeAndRecordLogs(jobs, handler).asserting { logs =>
        val summary = JobExecutionPolicy.errorSummary(error)
        verify(jobs).finish(job, Job.Failed, Some(summary))
        verify(jobs, never).defer(any[Job], any[Instant], any[Option[String]], any[Boolean])
        summary must include("MongoJsonParsingException")
        summary must include("financialSnapshots[0].metrics")
        logs mustBe List("warn" -> s"Job ${job.id} failed: $summary")
        List(summary, logs.map(_._2).mkString).foreach { value =>
          value must not include "api_key"
          value must not include "secret-value"
          value must not include "private-provider-response"
          value must not include "rawPayload"
        }
        succeed
      }
    }

    "cancel ongoing work when heartbeat renewal loses ownership without finishing the job" in {
      val jobs   = mock[JobRepository[IO]]
      val action = Action.SetSecurityActive(AAPL, false, java.time.Instant.EPOCH)
      val job    = Job(JobId("leased"), action, ts, ts, state = Job.Running, leaseToken = Some("owner"))
      when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturnSome(job)
      when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
      when(jobs.renew(eqTo(job), any[Instant], any[FiniteDuration])).thenReturnIO(false)
      when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
      (for {
        started   <- Deferred[IO, Unit]
        cancelled <- Deferred[IO, Unit]
        handler = new ActionHandler[IO] {
          def execute(action: Action): IO[ActionResult] =
            started.complete(()) >> IO.never[ActionResult].onCancel(cancelled.complete(()).void)
        }
        executor <- ActionExecutor.make(jobs, handler, _ => IO.pure(true), ActionExecutorConfig(1))
        _        <- executor.run
          .take(1)
          .compile
          .drain
          .start
          .bracket { fiber =>
            started.get >> fiber.joinWithNever >> cancelled.get
          }(_.cancel)
          .timeout(JobExecutionPolicy.HeartbeatInterval + 5.seconds)
      } yield ()).asserting { _ =>
        verify(jobs).renew(eqTo(job), any[Instant], any[FiniteDuration])
        verify(jobs, never).finish(any[Job], any[JobState], any[Option[String]])
        succeed
      }
    }
  }
