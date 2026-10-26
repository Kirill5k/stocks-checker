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
        firstRepo  <- ProviderBudgetRepository.make[IO](db)
        secondRepo <- ProviderBudgetRepository.make[IO](db)
        quality    <- DataQualityRepository.make[IO](db)
        first      <- ProviderAccess.make[IO](firstRepo, quality)
        second     <- ProviderAccess.make[IO](secondRepo, quality)
        calls      <- Ref.of[IO, Int](0)
        now        <- IO.realTimeInstant
        until   = now.plusSeconds(600).truncatedTo(ChronoUnit.MILLIS)
        request = calls.update(_ + 1) >> IO.raiseError[Unit](AppError.ProviderDeferred("shared", until, "Retry-After"))
        attempts <- (
          first.run("shared", ProviderLimits(1, 0, 2))(request).attempt,
          second.run("shared", ProviderLimits(1, 0, 2))(request).attempt
        ).parTupled
        issued        <- calls.get
        restartedRepo <- ProviderBudgetRepository.make[IO](db)
        restarted     <- ProviderAccess.make[IO](restartedRepo, quality)
        refused       <- restarted.run("shared", ProviderLimits(1, 0, 2))(calls.update(_ + 1)).attempt
        after         <- calls.get
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
        original      <- ProviderBudgetRepository.make[IO](db)
        quality       <- DataQualityRepository.make[IO](db)
        first         <- ProviderAccess.make[IO](original, quality)
        _             <- first.run("daily", ProviderLimits(60000, 1, 1))(IO.unit)
        _             <- IO.sleep(5.millis)
        restartedRepo <- ProviderBudgetRepository.make[IO](db)
        restarted     <- ProviderAccess.make[IO](restartedRepo, quality)
        now           <- IO.realTimeInstant
        second        <- restarted.run("daily", ProviderLimits(60000, 1, 1))(IO.unit).attempt
        reset = now.atZone(ZoneOffset.UTC).toLocalDate.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        tomorrow  <- restartedRepo.reserve("daily", reset, 1L, 1)
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
        one     <- ProviderBudgetRepository.make[IO](db)
        two     <- ProviderBudgetRepository.make[IO](db)
        _       <- (one.cooldown("Finnhub", now.plusSeconds(600)), two.cooldown("Finnhub", now.plusSeconds(60))).parTupled
        refused <- one.reserve("Finnhub", now, 1L, 100)
        other   <- two.reserve("TwelveData", now, 1L, 100)
      } yield {
        refused mustBe Some(now.plusSeconds(600))
        other mustBe None
      }
    }

    "retain independent credential budgets when a shared access service is recreated" in withEmbeddedMongoDatabase { db =>
      val limits = ProviderLimits(60000, 1, 1)
      for {
        repository          <- ProviderBudgetRepository.make[IO](db)
        quality             <- DataQualityRepository.make[IO](db)
        first               <- ProviderAccess.make[IO](repository, quality)
        _                   <- first.run("TwelveData", limits, Some("credential-one"))(IO.unit)
        restartedRepository <- ProviderBudgetRepository.make[IO](db)
        restarted           <- ProviderAccess.make[IO](restartedRepository, quality)
        firstKey            <- restarted.run("TwelveData", limits, Some("credential-one"))(IO.unit).attempt
        secondKey           <- restarted.run("TwelveData", limits, Some("credential-two"))(IO.pure(2))
        otherProvider       <- restarted.run("Finnhub", limits, Some("credential-one"))(IO.pure(3))
      } yield {
        firstKey.left.toOption.get mustBe a[AppError.ProviderDeferred]
        secondKey mustBe 2
        otherProvider mustBe 3
      }
    }

    "inherit legacy daily usage only when creating an account budget" in withEmbeddedMongoDatabase { db =>
      val now   = Instant.parse("2026-01-01T10:00:00Z")
      val reset = Instant.parse("2026-01-02T00:00:00Z")
      for {
        repository  <- ProviderBudgetRepository.make[IO](db)
        legacyFirst <- repository.reserve("TwelveData", now, 1L, 3)
        firstKey    <- repository.reserve("TwelveData", now.plusSeconds(1), 1L, 3, Some("first"))
        _           <- repository.reserve("TwelveData", now.plusSeconds(2), 1L, 3)
        _           <- repository.reserve("TwelveData", now.plusSeconds(3), 1L, 3)
        restarted   <- ProviderBudgetRepository.make[IO](db)
        existingKey <- restarted.reserve("TwelveData", now.plusSeconds(4), 1L, 3, Some("first"))
        exhausted   <- restarted.reserve("TwelveData", now.plusSeconds(5), 1L, 3, Some("first"))
        newKey      <- restarted.reserve("TwelveData", now.plusSeconds(5), 1L, 3, Some("second"))
        nextDay     <- restarted.reserve("TwelveData", reset, 1L, 3, Some("first"))
      } yield {
        legacyFirst mustBe None
        firstKey mustBe None
        existingKey mustBe None
        exhausted mustBe Some(reset)
        newKey mustBe Some(reset)
        nextDay mustBe None
      }
    }

    "inherit legacy cooldowns without overwriting an existing account's cooldown" in withEmbeddedMongoDatabase { db =>
      val now = Instant.parse("2026-01-01T10:00:00Z")
      for {
        repository    <- ProviderBudgetRepository.make[IO](db)
        _             <- repository.cooldown("TwelveData", now.plusSeconds(600))
        inherited     <- repository.reserve("TwelveData", now, 1L, 0, Some("first"))
        _             <- repository.cooldown("TwelveData", now.plusSeconds(1800))
        restarted     <- ProviderBudgetRepository.make[IO](db)
        unchanged     <- restarted.reserve("TwelveData", now, 1L, 0, Some("first"))
        _             <- restarted.cooldown("TwelveData", now.plusSeconds(1200), Some("first"))
        extended      <- restarted.reserve("TwelveData", now, 1L, 0, Some("first"))
        _             <- restarted.cooldown("TwelveData", now.plusSeconds(1200), Some("second"))
        newKey        <- restarted.reserve("TwelveData", now, 1L, 0, Some("second"))
        otherProvider <- restarted.reserve("Finnhub", now, 1L, 0, Some("first"))
      } yield {
        inherited mustBe Some(now.plusSeconds(600))
        unchanged mustBe inherited
        extended mustBe Some(now.plusSeconds(1200))
        newKey mustBe Some(now.plusSeconds(1800))
        otherProvider mustBe None
      }
    }
  }
}
