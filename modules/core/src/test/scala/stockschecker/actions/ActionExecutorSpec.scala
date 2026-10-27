package stockschecker.actions

import cats.effect.{Deferred, IO}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.{Job, JobId, JobState, JobStatusSummary}
import stockschecker.fixtures.*
import stockschecker.repositories.JobRepository

import java.time.Instant
import scala.concurrent.duration.*

class ActionExecutorSpec extends IOWordSpec:
  given Clock[IO]  = Clock.mock(ts)
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "An action executor" should {
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
