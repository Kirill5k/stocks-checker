package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*

class JobServiceSpec extends IOWordSpec:
  "JobService" should {
    "read an exact job through the dispatcher" in {
      val dispatcher = mock[ActionDispatcher[IO]]
      val id         = JobId("scheduled:3:1793599200000/245")
      val job        = Job(id, Action.RefreshInvestmentResearch, ts, ts, state = Job.Failed, error = Some("Data unavailable"))
      when(dispatcher.findJob(id)).thenReturnSome(job)
      JobService.make(dispatcher).flatMap(_.get(id)).asserting { result =>
        result mustBe JobView.from(job)
        verify(dispatcher).findJob(id)
        verifyNoMoreInteractions(dispatcher)
        succeed
      }
    }

    "return a not-found error for missing or pruned jobs" in {
      val dispatcher = mock[ActionDispatcher[IO]]
      val id         = JobId("pruned/1")
      when(dispatcher.findJob(id)).thenReturnNone
      JobService.make(dispatcher).flatMap(_.get(id)).attempt.asserting { result =>
        result mustBe Left(AppError.EntityDoesNotExist("Job", id.value))
      }
    }

    "preserve read failures" in {
      val dispatcher = mock[ActionDispatcher[IO]]
      val id         = JobId("unavailable/0")
      val error      = new java.io.IOException("Connection unavailable")
      when(dispatcher.findJob(id)).thenRaiseError(error)
      when(dispatcher.listJobs(20, JobFilter())).thenRaiseError(error)
      (for
        service <- JobService.make(dispatcher)
        single  <- service.get(id).attempt
        listed  <- service.list(20, JobFilter()).attempt
      yield (single, listed)).asserting { case (single, listed) =>
        single mustBe Left(error)
        listed mustBe Left(error)
      }
    }

    "forward all list filters through the dispatcher" in {
      val dispatcher = mock[ActionDispatcher[IO]]
      val filter     = JobFilter(Some(FetchLatestSecuritiesCommand.id), Some(Job.Partial), Some("refresh-investment-research"))
      val job        = Job(JobId("run"), Action.RefreshInvestmentResearch, ts, ts, state = Job.Partial)
      when(dispatcher.listJobs(100, filter)).thenReturnIO(List(job))
      JobService.make(dispatcher).flatMap(_.list(100, filter)).asserting { result =>
        result mustBe List(JobView.from(job))
        verify(dispatcher).listJobs(100, filter)
        verifyNoMoreInteractions(dispatcher)
        succeed
      }
    }

  }
