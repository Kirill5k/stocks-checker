package stockschecker.actions

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.mockito.ArgumentCaptor
import stockschecker.domain.{Job, JobId}
import stockschecker.fixtures.*
import stockschecker.repositories.JobRepository

import java.time.LocalDate

class ActionDispatcherSpec extends IOWordSpec {
  given Clock[IO] = Clock.mock(ts)

  "An ActionDispatcher" should {
    "persist accepted work before returning" in {
      val jobs   = mock[JobRepository[IO]]
      val action = Action.SetSecurityActive(AAPL, false, java.time.Instant.EPOCH)
      when(jobs.enqueue(any[Job], any[Int])).thenReturnUnit
      val captured = ArgumentCaptor.forClass(classOf[Job])
      (for {
        dispatcher <- ActionDispatcher.make(jobs)
        _          <- dispatcher.dispatch(action)
      } yield ()).asserting { _ =>
        verify(jobs).enqueue(captured.capture(), eqTo(ActionPriority.Internal))
        captured.getValue.action mustBe action
        captured.getValue.availableAt mustBe ts
        captured.getValue.createdAt mustBe ts
        captured.getValue.id.value must not be empty
      }
    }
    "derive evaluation priority while preserving the caller's deterministic ID" in {
      val jobs   = mock[JobRepository[IO]]
      val id     = JobId("evaluation-job")
      val action = Action.RefreshEvaluationPrices(AAPL, LocalDate.parse("2026-09-30"))
      when(jobs.enqueue(any[Job], any[Int])).thenReturnUnit
      (for
        dispatcher <- ActionDispatcher.make(jobs)
        _          <- dispatcher.dispatchOnce(id, action)
      yield ()).asserting { _ =>
        verify(jobs).enqueue(Job(id, action, ts, ts), ActionPriority.Evaluation)
        succeed
      }
    }

  }
}
