package stockschecker.clients

import kirill5k.common.cats.Clock
import cats.effect.{Deferred, IO}
import cats.syntax.traverse.*
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.errors.AppError
import stockschecker.repositories.ProviderBudgetRepository

import java.time.Instant

class ProviderAccessSpec extends IOWordSpec {
  given Clock[IO]  = Clock.make[IO]
  private val fast = ProviderLimits(requestsPerMinute = 60000, dailyAllowance = 0, maxConcurrent = 1)

  private def budgets: ProviderBudgetRepository[IO] = {
    val repository = mock[ProviderBudgetRepository[IO]]
    when(repository.reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]])).thenReturnIO(None)
    when(repository.cooldown(anyString, any[Instant], any[Option[String]])).thenReturnUnit
    repository
  }

  "ProviderAccess" should {
    "persist provider cooldowns and honor shared reservations without issuing rejected requests" in {
      val repository = budgets
      (for
        now <- IO.realTimeInstant
        until = now.plusSeconds(300)
        _     = when(repository.reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]]))
          .thenReturn(IO.pure(None), IO.pure(Some(until)))
        access <- ProviderAccess.make[IO](repository)
        _      <- access.run("test", fast)(IO.raiseError[Unit](AppError.ProviderDeferred("test", until, "Retry-After"))).attempt
        result <- access.run("test", fast)(IO.raiseError[Unit](new AssertionError("Rejected request ran"))).attempt
      yield (result, until)).asserting { case (result, until) =>
        result.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(until)
        verify(repository).cooldown("test", until, None)
        succeed
      }
    }

    "release concurrency permits after both failure and cancellation" in
      (for
        access       <- ProviderAccess.make[IO](budgets)
        started      <- Deferred[IO, Unit]
        active       <- access.run("test", fast)(started.complete(()).void >> IO.never[Unit]).start
        _            <- started.get
        blocked      <- access.run("test", fast)(IO.unit).attempt
        _            <- active.cancel
        afterCancel  <- access.run("test", fast)(IO.pure(1)).attempt
        failed       <- access.run("test", fast)(IO.raiseError[Unit](new java.io.IOException("offline"))).attempt
        afterFailure <- access.run("test", fast)(IO.pure(2)).attempt
      yield (blocked, afterCancel, failed, afterFailure)).asserting { case (blocked, afterCancel, failed, afterFailure) =>
        blocked.left.toOption.exists(_.isInstanceOf[AppError.ProviderDeferred]) mustBe true
        afterCancel mustBe Right(1)
        failed.isLeft mustBe true
        afterFailure mustBe Right(2)
      }

    "pause authentication failures until the access service is recreated" in {
      val repository = budgets
      (for
        access    <- ProviderAccess.make[IO](repository)
        first     <- access.run("test", fast)(IO.raiseError[Unit](AppError.ProviderConfiguration("test", "unauthorized"))).attempt
        second    <- access.run("test", fast)(IO.unit).attempt
        restarted <- ProviderAccess.make[IO](repository)
        resumed   <- restarted.run("test", fast)(IO.pure(1)).attempt
      yield (first, second, resumed)).asserting { case (first, second, resumed) =>
        first.left.toOption.exists(_.isInstanceOf[AppError.ProviderConfiguration]) mustBe true
        second.left.toOption.exists(_.isInstanceOf[AppError.ProviderConfiguration]) mustBe true
        resumed mustBe Right(1)
        verify(repository, times(2)).reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]])
        verify(repository, never).cooldown(anyString, any[Instant], any[Option[String]])
        succeed
      }
    }

    "release concurrency permits when the shared budget repository fails" in {
      val repository = budgets
      when(repository.reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]]))
        .thenReturn(IO.raiseError(new java.io.IOException("offline")), IO.pure(None))
      (for
        access    <- ProviderAccess.make[IO](repository)
        failed    <- access.run("test", fast)(IO.unit).attempt
        recovered <- access.run("test", fast)(IO.pure(1)).attempt
      yield (failed, recovered)).asserting { case (failed, recovered) =>
        failed.left.toOption.get mustBe a[java.io.IOException]
        recovered mustBe Right(1)
      }
    }

    "reject invalid quota configuration" in
      List(fast.copy(requestsPerMinute = 0), fast.copy(maxConcurrent = 0), fast.copy(dailyAllowance = -1))
        .traverse(limits => ProviderAccess.make[IO](budgets).flatMap(_.validateLimits("test", limits)).attempt)
        .asserting(results => results.forall(_.left.toOption.exists(_.isInstanceOf[IllegalArgumentException])) mustBe true)

    "isolate concurrency and credential pauses between providers and accounts" in {
      val repository = budgets
      (for
        access        <- ProviderAccess.make[IO](repository)
        started       <- Deferred[IO, Unit]
        active        <- access.run("TwelveData", fast, Some("first"))(started.complete(()).void >> IO.never[Unit]).start
        _             <- started.get
        same          <- access.run("TwelveData", fast, Some("first"))(IO.unit).attempt
        otherKey      <- access.run("TwelveData", fast, Some("second"))(IO.pure(1))
        otherProvider <- access.run("Finnhub", fast)(IO.pure(2))
        _             <- active.cancel
        _             <- access
          .run("TwelveData", fast, Some("first"))(
            IO.raiseError[Unit](AppError.ProviderConfiguration("TwelveData", "expired"))
          )
          .attempt
        _             <- access.run("Finnhub", fast)(IO.raiseError[Unit](AppError.ProviderConfiguration("Finnhub", "expired"))).attempt
        healthyKey    <- access.run("TwelveData", fast, Some("second"))(IO.pure(3))
        disabledKey   <- access.run("TwelveData", fast, Some("first"))(IO.unit).attempt
        stillDisabled <- access.run("Finnhub", fast)(IO.unit).attempt
      yield (same, otherKey, otherProvider, healthyKey, disabledKey, stillDisabled)).asserting {
        case (same, otherKey, otherProvider, healthyKey, disabledKey, stillDisabled) =>
          same.left.toOption.get mustBe a[AppError.ProviderDeferred]
          List(otherKey, otherProvider, healthyKey) mustBe List(1, 2, 3)
          disabledKey.left.toOption.get mustBe a[AppError.ProviderConfiguration]
          stillDisabled.left.toOption.get mustBe a[AppError.ProviderConfiguration]
          verify(repository, times(2)).reserve(
            org.mockito.ArgumentMatchers.eq("TwelveData"),
            any[Instant],
            any[Long],
            any[Int],
            org.mockito.ArgumentMatchers.eq(Some("first"))
          )
          verify(repository, times(2)).reserve(
            org.mockito.ArgumentMatchers.eq("TwelveData"),
            any[Instant],
            any[Long],
            any[Int],
            org.mockito.ArgumentMatchers.eq(Some("second"))
          )
          succeed
      }
    }

    "release a permit when cancellation interrupts budget reservation" in {
      val repository = budgets
      (for
        started <- Deferred[IO, Unit]
        _ = when(repository.reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]]))
          .thenReturn(started.complete(()).void >> IO.never[Option[Instant]], IO.pure(None))
        access <- ProviderAccess.make[IO](repository)
        active <- access.run("test", fast)(IO.unit).start
        _      <- started.get
        _      <- active.cancel
        result <- access.run("test", fast)(IO.pure(1))
      yield result).asserting(_ mustBe 1)
    }
  }
}
