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
    "link manual runs for cleanup without treating them as scheduled occurrences" in {
      val jobs    = mock[JobRepository[IO]]
      val command = FetchLatestSecuritiesCommand
      when(jobs.enqueue(any[Job], any[Int])).thenReturnUnit
      val captured = ArgumentCaptor.forClass(classOf[Job])
      ActionDispatcher.make(jobs).flatMap(_.dispatchForCommand(command.id, command.action)).asserting { id =>
        verify(jobs).enqueue(captured.capture(), eqTo(ActionPriority.forAction(command.action)))
        val job = captured.getValue
        job.id mustBe id
        job.sourceCommandId mustBe Some(command.id)
        job.command mustBe None
        job.action mustBe command.action
      }
    }

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

    "return the durable id from tracked dispatch" in {
      val jobs   = mock[JobRepository[IO]]
      val action = Action.RefreshInvestmentResearch
      when(jobs.enqueue(any[Job], any[Int])).thenReturnUnit
      val captured = ArgumentCaptor.forClass(classOf[Job])
      ActionDispatcher.make(jobs).flatMap(_.dispatchTracked(action)).asserting { id =>
        verify(jobs).enqueue(captured.capture(), eqTo(ActionPriority.forAction(action)))
        captured.getValue.id mustBe id
        captured.getValue.action mustBe action
      }
    }

    "read optional job status through the job repository" in {
      val jobs = mock[JobRepository[IO]]
      val id   = JobId("run/0")
      val job  = Job(id, Action.RefreshInvestmentResearch, ts, ts)
      when(jobs.find(id)).thenReturnSome(job)
      (for
        dispatcher <- ActionDispatcher.make(jobs)
        found      <- dispatcher.findJob(id)
      yield found).asserting { found =>
        found mustBe Some(job)
        verify(jobs).find(id)
        succeed
      }
    }

  }
}
