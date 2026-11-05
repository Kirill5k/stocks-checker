package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.JobRepository

class JobServiceSpec extends IOWordSpec:
  "JobService" should {
    "propagate the repository's not-found error for missing or pruned jobs" in {
      val repository = mock[JobRepository[IO]]
      val id         = JobId("pruned/1")
      val error      = AppError.EntityDoesNotExist("Job", id.value)
      when(repository.get(id)).thenRaiseError(error)
      JobService.make(repository).flatMap(_.get(id)).attempt.asserting { result =>
        result mustBe Left(error)
      }
    }

  }
