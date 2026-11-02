package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.ActionDispatcher
import stockschecker.domain.*
import stockschecker.domain.errors.AppError

class JobServiceSpec extends IOWordSpec:
  "JobService" should {
    "return a not-found error for missing or pruned jobs" in {
      val dispatcher = mock[ActionDispatcher[IO]]
      val id         = JobId("pruned/1")
      when(dispatcher.findJob(id)).thenReturnNone
      JobService.make(dispatcher).flatMap(_.get(id)).attempt.asserting { result =>
        result mustBe Left(AppError.EntityDoesNotExist("Job", id.value))
      }
    }

  }
