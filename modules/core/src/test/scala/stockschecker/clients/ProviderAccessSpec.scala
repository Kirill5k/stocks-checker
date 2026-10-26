package stockschecker.clients

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.errors.AppError

import java.time.ZoneOffset
import scala.concurrent.duration.*

class ProviderAccessSpec extends IOWordSpec {
  private val fast = ProviderLimits(requestsPerMinute = 60000, maxConcurrent = 1)

  "ProviderAccess" should {
    "share a response cooldown across callers without issuing rejected requests" in {
      (for {
        access <- ProviderAccess.make[IO]("test", fast)
        now <- IO.realTimeInstant
        calls <- Ref.of[IO, Int](0)
        until = now.plusSeconds(300)
        _ <- access.run(IO.raiseError[Unit](AppError.ProviderDeferred("test", until, "Retry-After"))).attempt
        result <- access.run(calls.update(_ + 1)).attempt
        count <- calls.get
      } yield (result, count, until)).asserting { case (result, count, until) =>
        result.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(until)
        count mustBe 0
      }
    }

    "release concurrency permits after both failure and cancellation" in {
      (for {
        access <- ProviderAccess.make[IO]("test", fast)
        started <- Deferred[IO, Unit]
        active <- access.run(started.complete(()).void >> IO.never[Unit]).start
        _ <- started.get
        _ <- IO.sleep(5.millis)
        blocked <- access.run(IO.unit).attempt
        _ <- active.cancel
        _ <- IO.sleep(5.millis)
        afterCancel <- access.run(IO.pure(1)).attempt
        _ <- IO.sleep(5.millis)
        failed <- access.run(IO.raiseError[Unit](new java.io.IOException("offline"))).attempt
        _ <- IO.sleep(5.millis)
        afterFailure <- access.run(IO.pure(2)).attempt
      } yield (blocked, afterCancel, failed, afterFailure)).asserting { case (blocked, afterCancel, failed, afterFailure) =>
        blocked.left.toOption.exists(_.isInstanceOf[AppError.ProviderDeferred]) mustBe true
        afterCancel mustBe Right(1)
        failed.isLeft mustBe true
        afterFailure mustBe Right(2)
      }
    }

    "enforce the daily quota until the next UTC day" in {
      (for {
        access <- ProviderAccess.make[IO]("test", fast.copy(dailyAllowance = 1))
        _ <- access.run(IO.unit)
        _ <- IO.sleep(5.millis)
        now <- IO.realTimeInstant
        result <- access.run(IO.unit).attempt
      } yield (now, result)).asserting { case (now, result) =>
        val reset = now.atZone(ZoneOffset.UTC).toLocalDate.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        result.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(reset)
      }
    }

    "pause the first authentication failure as well as subsequent callers" in {
      (for {
        access <- ProviderAccess.make[IO]("test", fast)
        first <- access.run(IO.raiseError[Unit](AppError.HttpClient("test", 401, "unauthorized"))).attempt
        second <- access.run(IO.unit).attempt
      } yield (first, second)).asserting { case (first, second) =>
        first.left.toOption.exists(_.isInstanceOf[AppError.ProviderConfiguration]) mustBe true
        second.left.toOption.exists(_.isInstanceOf[AppError.ProviderConfiguration]) mustBe true
      }
    }

    "never shorten a longer cooldown when concurrent requests return 429" in {
      (for {
        access <- ProviderAccess.make[IO]("test", fast.copy(maxConcurrent = 2))
        now <- IO.realTimeInstant
        until = now.plusSeconds(600)
        startedA <- Deferred[IO, Unit]
        startedB <- Deferred[IO, Unit]
        releaseA <- Deferred[IO, Unit]
        releaseB <- Deferred[IO, Unit]
        a <- access.run(startedA.complete(()).void >> releaseA.get >> IO.raiseError[Unit](AppError.ProviderDeferred("test", until, "Retry-After"))).attempt.start
        _ <- startedA.get
        _ <- IO.sleep(5.millis)
        b <- access.run(startedB.complete(()).void >> releaseB.get >> IO.raiseError[Unit](AppError.HttpClient("test", 429, "rate limited"))).attempt.start
        _ <- startedB.get.timeout(2.seconds)
        _ <- releaseA.complete(())
        _ <- a.joinWithNever
        _ <- releaseB.complete(())
        _ <- b.joinWithNever
        result <- access.run(IO.unit).attempt
      } yield (until, result)).asserting { case (until, result) =>
        result.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(until)
      }
    }

    "reject invalid quota configuration" in {
      List(fast.copy(requestsPerMinute = 0), fast.copy(maxConcurrent = 0), fast.copy(dailyAllowance = -1))
        .traverse(limits => ProviderAccess.make[IO]("test", limits).attempt)
        .asserting(results => results.forall(_.left.toOption.exists(_.isInstanceOf[IllegalArgumentException])) mustBe true)
    }
  }
}
