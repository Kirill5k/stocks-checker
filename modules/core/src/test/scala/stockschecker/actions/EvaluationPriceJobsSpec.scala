package stockschecker.actions

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.{Job, JobId, JobPlan}
import stockschecker.fixtures.AAPL
import stockschecker.repositories.JobRepository

import java.time.{Instant, LocalDate}

class EvaluationPriceJobsSpec extends IOWordSpec:
  private val now     = Instant.parse("2026-10-19T12:00:00Z")
  private val through = LocalDate.parse("2026-09-30")
  private val id      = JobId("f5b1cbea-b982-39b3-be1e-f63f18105f70")
  private val action  = Action.RefreshEvaluationPrices(AAPL, LocalDate.parse("2013-09-30"), through)

  "Evaluation price jobs" should {
    "preserve the persisted canonical job ID, requested range and evaluation priority" in {
      val repository = mock[JobRepository[IO]]
      when(repository.find(id)).thenReturnNone
      when(repository.enqueue(any[Job], any[Int])).thenReturnUnit
      (for
        jobs   <- EvaluationPriceJobs.make(repository)
        result <- jobs.ensure(AAPL, through, now)
      yield result).asserting { result =>
        result mustBe Some(id)
        verify(repository).enqueue(Job(id, action, now, now), ActionPriority.Evaluation)
        succeed
      }
    }

    "return an existing pending job without inserting or restarting it" in {
      val repository = mock[JobRepository[IO]]
      when(repository.find(id)).thenReturnSome(Job(id, action, now, now, state = Job.Paused))
      (for
        jobs    <- EvaluationPriceJobs.make(repository)
        pending <- jobs.pending(AAPL, through)
        ensured <- jobs.ensure(AAPL, through, now)
      yield (pending, ensured)).asserting { result =>
        result mustBe (Some(id), Some(id))
        verify(repository, never).enqueue(any[Job], any[Int])
        verify(repository, never).restart(any[Job], any[Instant])
        succeed
      }
    }

    "schedule a successful but incomplete vintage for repair after its daily cooldown" in {
      val repository = mock[JobRepository[IO]]
      val completed  = now.minusSeconds(60)
      val previous   = Job(id, action, now, now, state = Job.Succeeded, completedAt = Some(completed))
      when(repository.find(id)).thenReturnSome(previous)
      when(repository.restart(previous, completed.plusSeconds(86400))).thenReturnIO(true)
      (for
        jobs   <- EvaluationPriceJobs.make(repository)
        result <- jobs.ensure(AAPL, through, now)
      yield result).asserting { result =>
        result mustBe Some(id)
        verify(repository).restart(previous, completed.plusSeconds(86400))
        succeed
      }
    }

    "observe another caller's pending repair after losing the restart comparison" in {
      val repository = mock[JobRepository[IO]]
      val previous   = Job(id, action, now, now, state = Job.Failed, completedAt = Some(now.minusSeconds(172800)))
      when(repository.find(id)).thenReturn(IO.pure(Some(previous)), IO.pure(Some(previous.copy(state = Job.Pending))))
      when(repository.restart(previous, now)).thenReturnIO(false)
      (for
        jobs   <- EvaluationPriceJobs.make(repository)
        result <- jobs.ensure(AAPL, through, now)
      yield result).asserting(_ mustBe Some(id))
    }

    "leave terminal jobs untouched when the repair budget is exhausted or the job is not a standalone repair" in {
      val previous = Job(id, action, now, now, state = Job.Failed)
      val rejected = List(
        previous.copy(restartCount = Some(EvaluationPriceJobs.MaxRestarts)),
        previous.copy(children = List(JobId("child"))),
        previous.copy(plan = Some(JobPlan(List(action), false))),
        previous.copy(id = JobId("parent/0")),
        previous.copy(action = Action.DeactivateSecurity(AAPL))
      )
      import cats.syntax.traverse.*
      rejected
        .traverse { job =>
          val repository = mock[JobRepository[IO]]
          when(repository.find(id)).thenReturnSome(job)
          (for
            jobs    <- EvaluationPriceJobs.make(repository)
            pending <- jobs.pending(AAPL, through)
            ensured <- jobs.ensure(AAPL, through, now)
          yield (pending, ensured)).map { result =>
            result mustBe (None, None)
            verify(repository, never).restart(any[Job], any[Instant])
            verify(repository, never).enqueue(any[Job], any[Int])
            succeed
          }
        }
        .asserting(_ => succeed)
    }
  }
