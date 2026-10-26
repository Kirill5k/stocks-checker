package stockschecker.repositories

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import stockschecker.clients.{ProviderAccess, ProviderLimits}
import stockschecker.domain.errors.AppError

import java.time.{Instant, ZoneOffset}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class ProviderBudgetRepositorySpec extends RepositorySpec {
  override def port: Int = 12162

  "Persistent provider budgets" should {
    "atomically share admission between two gates and retain response cooldown after restart" in withEmbeddedMongoDatabase { db =>
      for {
        firstRepo <- ProviderBudgetRepository.make[IO](db)
        secondRepo <- ProviderBudgetRepository.make[IO](db)
        first <- ProviderAccess.make[IO]("shared", ProviderLimits(1, 0, 2), Some(firstRepo))
        second <- ProviderAccess.make[IO]("shared", ProviderLimits(1, 0, 2), Some(secondRepo))
        calls <- Ref.of[IO, Int](0)
        now <- IO.realTimeInstant
        until = now.plusSeconds(600).truncatedTo(ChronoUnit.MILLIS)
        request = calls.update(_ + 1) >> IO.raiseError[Unit](AppError.ProviderDeferred("shared", until, "Retry-After"))
        attempts <- (first.run(request).attempt, second.run(request).attempt).parTupled
        issued <- calls.get
        restartedRepo <- ProviderBudgetRepository.make[IO](db)
        restarted <- ProviderAccess.make[IO]("shared", ProviderLimits(1, 0, 2), Some(restartedRepo))
        refused <- restarted.run(calls.update(_ + 1)).attempt
        after <- calls.get
      } yield {
        attempts._1.isLeft mustBe true
        attempts._2.isLeft mustBe true
        issued mustBe 1
        after mustBe 1
        refused.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(until)
      }
    }

    "preserve the daily allowance across new gates and permit the next UTC day" in withEmbeddedMongoDatabase { db =>
      for {
        original <- ProviderBudgetRepository.make[IO](db)
        first <- ProviderAccess.make[IO]("daily", ProviderLimits(60000, 1, 1), Some(original))
        _ <- first.run(IO.unit)
        _ <- IO.sleep(5.millis)
        restartedRepo <- ProviderBudgetRepository.make[IO](db)
        restarted <- ProviderAccess.make[IO]("daily", ProviderLimits(60000, 1, 1), Some(restartedRepo))
        now <- IO.realTimeInstant
        second <- restarted.run(IO.unit).attempt
        reset = now.atZone(ZoneOffset.UTC).toLocalDate.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        tomorrow <- restartedRepo.reserve("daily", reset, 1L, 1)
        exhausted <- restartedRepo.reserve("daily", reset.plusSeconds(1), 1L, 1)
      } yield {
        second.left.toOption.collect { case e: AppError.ProviderDeferred => e.retryAt } mustBe Some(reset)
        tomorrow mustBe None
        exhausted mustBe Some(reset.plusSeconds(86400))
      }
    }

    "retain the longest concurrent cooldown and isolate provider accounts" in withEmbeddedMongoDatabase { db =>
      val now = Instant.parse("2026-01-01T10:00:00Z")
      for {
        one <- ProviderBudgetRepository.make[IO](db)
        two <- ProviderBudgetRepository.make[IO](db)
        _ <- (one.cooldown("Finnhub", now.plusSeconds(600)), two.cooldown("Finnhub", now.plusSeconds(60))).parTupled
        refused <- one.reserve("Finnhub", now, 1L, 100)
        other <- two.reserve("TwelveData", now, 1L, 100)
      } yield {
        refused mustBe Some(now.plusSeconds(600))
        other mustBe None
      }
    }
  }
}
