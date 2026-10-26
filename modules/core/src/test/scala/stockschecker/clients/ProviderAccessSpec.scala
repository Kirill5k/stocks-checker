package stockschecker.clients

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.errors.AppError
import stockschecker.repositories.ProviderBudgetRepository

import java.time.Instant

class ProviderAccessSpec extends IOWordSpec {
  private val fast = ProviderLimits(requestsPerMinute = 60000, dailyAllowance = 0, maxConcurrent = 1)

  private def budgets: ProviderBudgetRepository[IO] = {
    val repository = mock[ProviderBudgetRepository[IO]]
    when(repository.reserve(anyString, any[Instant], any[Long], any[Int])).thenReturnIO(None)
    when(repository.cooldown(anyString, any[Instant])).thenReturnUnit
    repository
  }

  "ProviderAccess" should {
    "persist provider cooldowns and honor shared reservations without issuing rejected requests" in {
      val repository = budgets
      (for
        now <- IO.realTimeInstant
        until = now.plusSeconds(300)
        _     = when(repository.reserve(anyString, any[Instant], any[Long], any[Int])).thenReturn(IO.pure(None), IO.pure(Some(until)))
        access <- ProviderAccess.make[IO]("test", fast, repository)
        _      <- access.run(IO.raiseError[Unit](AppError.ProviderDeferred("test", until, "Retry-After"))).attempt
        result <- access.run(IO.raiseError[Unit](new AssertionError("Rejected request ran"))).attempt
      yield (result, until)).asserting { case (result, until) =>
        result.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(until)
        verify(repository).cooldown("test", until)
        succeed
      }
    }

    "release concurrency permits after both failure and cancellation" in
      (for
        access       <- ProviderAccess.make[IO]("test", fast, budgets)
        started      <- Deferred[IO, Unit]
        active       <- access.run(started.complete(()).void >> IO.never[Unit]).start
        _            <- started.get
        blocked      <- access.run(IO.unit).attempt
        _            <- active.cancel
        afterCancel  <- access.run(IO.pure(1)).attempt
        failed       <- access.run(IO.raiseError[Unit](new java.io.IOException("offline"))).attempt
        afterFailure <- access.run(IO.pure(2)).attempt
      yield (blocked, afterCancel, failed, afterFailure)).asserting { case (blocked, afterCancel, failed, afterFailure) =>
        blocked.left.toOption.exists(_.isInstanceOf[AppError.ProviderDeferred]) mustBe true
        afterCancel mustBe Right(1)
        failed.isLeft mustBe true
        afterFailure mustBe Right(2)
      }

    "pause authentication failures and resume without resetting shared quotas" in {
      val repository = budgets
      (for
        access  <- ProviderAccess.make[IO]("test", fast, repository)
        first   <- access.run(IO.raiseError[Unit](AppError.ProviderConfiguration("test", "unauthorized"))).attempt
        second  <- access.run(IO.unit).attempt
        _       <- access.resume
        resumed <- access.run(IO.pure(1)).attempt
      yield (first, second, resumed)).asserting { case (first, second, resumed) =>
        first.left.toOption.exists(_.isInstanceOf[AppError.ProviderConfiguration]) mustBe true
        second.left.toOption.exists(_.isInstanceOf[AppError.ProviderConfiguration]) mustBe true
        resumed mustBe Right(1)
        verify(repository, times(2)).reserve(anyString, any[Instant], any[Long], any[Int])
        verify(repository, never).cooldown(anyString, any[Instant])
        succeed
      }
    }

    "release concurrency permits when the shared budget repository fails" in {
      val repository = budgets
      when(repository.reserve(anyString, any[Instant], any[Long], any[Int]))
        .thenReturn(IO.raiseError(new java.io.IOException("offline")), IO.pure(None))
      (for
        access    <- ProviderAccess.make[IO]("test", fast, repository)
        failed    <- access.run(IO.unit).attempt
        recovered <- access.run(IO.pure(1)).attempt
      yield (failed, recovered)).asserting { case (failed, recovered) =>
        failed.left.toOption.get mustBe a[java.io.IOException]
        recovered mustBe Right(1)
      }
    }

    "reject invalid quota configuration" in
      List(fast.copy(requestsPerMinute = 0), fast.copy(maxConcurrent = 0), fast.copy(dailyAllowance = -1))
        .traverse(limits => ProviderAccess.make[IO]("test", limits, budgets).attempt)
        .asserting(results => results.forall(_.left.toOption.exists(_.isInstanceOf[IllegalArgumentException])) mustBe true)
  }
}
