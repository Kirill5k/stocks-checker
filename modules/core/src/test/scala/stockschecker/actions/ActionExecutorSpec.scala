package stockschecker.actions

import cats.effect.{Deferred, IO, Ref}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
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
        executor <- ActionExecutor.make(jobs, handler, ActionExecutorConfig(1))
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
        waitCount = 30,
        deferralCount = 3,
        leaseToken = Some("owner")
      )
      val error = AppError.ProviderDeferred(
        Provider.TwelveData,
        ts.plusSeconds(1),
        ProviderDeferralReason.ConcurrencyLimit,
        Some("time_series for AAPL")
      )
      val scheduled = ArgumentCaptor.forClass(classOf[Instant])
      when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturn(IO.pure(Some(job)), IO.pure(None))
      when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
      when(jobs.defer(any[Job], any[Instant], any[Option[String]], any[Boolean], any[Boolean])).thenReturnUnit
      when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
      val handler = new ActionHandler[IO] {
        def execute(action: Action): IO[ActionResult] = IO.raiseError(error)
      }
      executeAndRecordLogs(jobs, handler).asserting { logs =>
        verify(jobs).defer(eqTo(job), scheduled.capture(), eqTo(Some(error.getMessage)), eqTo(false), eqTo(true))
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
        verify(jobs).finish(job, Job.Failed, Some(error.getMessage))
        verify(jobs, never).defer(any[Job], any[Instant], any[Option[String]], any[Boolean], any[Boolean])
        logs mustBe List("warn" -> s"Job ${job.id} failed: ${error.getMessage}")
        succeed
      }
    }

    List(
      AppError.DataUnavailable(AAPL, "no matching recent trading session"),
      AppError.CompanyProfileNotFound(AAPL)
    ).foreach { error =>
      s"record ${error.getClass.getSimpleName} as a terminal failure at informational level" in {
        val jobs = mock[JobRepository[IO]]
        val job  = Job(JobId("unavailable"), Action.RefreshInvestmentResearch, ts, ts, state = Job.Running, leaseToken = Some("owner"))
        when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturn(IO.pure(Some(job)), IO.pure(None))
        when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
        when(jobs.finish(any[Job], any[JobState], any[Option[String]])).thenReturnUnit
        when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
        val handler = new ActionHandler[IO] {
          def execute(action: Action): IO[ActionResult] = IO.raiseError(error)
        }
        executeAndRecordLogs(jobs, handler).asserting { logs =>
          verify(jobs).finish(job, Job.Failed, Some(error.getMessage))
          verify(jobs, never).defer(any[Job], any[Instant], any[Option[String]], any[Boolean], any[Boolean])
          logs mustBe List("info" -> s"Job ${job.id} failed: ${error.getMessage}")
          succeed
        }
      }
    }

    "retry a failed completion write without executing the action again" in {
      val jobs = mock[JobRepository[IO]]
      val job  = Job(JobId("finish-retry"), Action.RefreshInvestmentResearch, ts, ts, state = Job.Running, leaseToken = Some("owner"))
      when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturn(IO.pure(Some(job)), IO.pure(None))
      when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
      when(jobs.finish(job, Job.Succeeded, None)).thenReturn(IO.raiseError(new java.io.IOException("write interrupted")), IO.unit)
      when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
      (for
        calls <- Ref.of[IO, Int](0)
        handler = new ActionHandler[IO] {
          def execute(action: Action): IO[ActionResult] = calls.update(_ + 1).as(ActionResult.Completed)
        }
        _     <- executeAndRecordLogs(jobs, handler)
        count <- calls.get
      yield count).asserting { count =>
        count mustBe 1
        verify(jobs, times(2)).finish(job, Job.Succeeded, None)
        verify(jobs, never).defer(any[Job], any[Instant], any[Option[String]], any[Boolean], any[Boolean])
        succeed
      }
    }

    "keep the action running when a failed heartbeat succeeds before lease expiry" in {
      val jobs = mock[JobRepository[IO]]
      val job  = Job(
        JobId("heartbeat-retry"),
        Action.RefreshInvestmentResearch,
        ts,
        ts,
        state = Job.Running,
        leaseToken = Some("owner"),
        leaseUntil = Some(ts.plusSeconds(120))
      )
      when(jobs.claim(any[Instant], any[FiniteDuration])).thenReturn(IO.pure(Some(job)), IO.pure(None))
      when(jobs.summarize(Nil)).thenReturnIO(JobStatusSummary(Map.empty))
      when(jobs.finish(job, Job.Succeeded, None)).thenReturnUnit
      when(jobs.pruneCompleted(any[Instant])).thenReturnUnit
      (for
        renewed   <- Deferred[IO, Unit]
        cancelled <- Ref.of[IO, Boolean](false)
        _ = when(jobs.renew(eqTo(job), any[Instant], any[FiniteDuration]))
          .thenReturn(IO.raiseError(new java.io.IOException("temporary outage")), renewed.complete(()).as(true))
        handler = new ActionHandler[IO] {
          def execute(action: Action): IO[ActionResult] = renewed.get.as(ActionResult.Completed).onCancel(cancelled.set(true))
        }
        executor     <- ActionExecutor.make(jobs, handler, ActionExecutorConfig(1))
        _            <- executor.run.take(1).compile.drain.timeout(JobExecutionPolicy.HeartbeatInterval + 5.seconds)
        wasCancelled <- cancelled.get
      yield wasCancelled).asserting { wasCancelled =>
        wasCancelled mustBe false
        verify(jobs, times(2)).renew(eqTo(job), any[Instant], any[FiniteDuration])
        verify(jobs).finish(job, Job.Succeeded, None)
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
        executor <- ActionExecutor.make(jobs, handler, ActionExecutorConfig(1))
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
