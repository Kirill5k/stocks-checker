package stockschecker.repositories

import stockschecker.domain.Provider
import kirill5k.common.cats.Clock
import cats.effect.{IO, Ref}
import cats.syntax.parallel.*
import stockschecker.clients.{ProviderAccess, ProviderLimits}
import stockschecker.domain.errors.AppError

import java.time.{Instant, ZoneOffset}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class ProviderBudgetRepositorySpec extends RepositorySpec {
  given Clock[IO]        = Clock.make[IO]
  override def port: Int = 12162

  "Persistent provider budgets" should {
    "atomically share admission between two gates and retain response cooldown after restart" in withEmbeddedMongoDatabase { db =>
      for {
        firstRepo  <- ProviderBudgetRepository.make[IO](db)
        secondRepo <- ProviderBudgetRepository.make[IO](db)
        first      <- ProviderAccess.make[IO](firstRepo)
        second     <- ProviderAccess.make[IO](secondRepo)
        calls      <- Ref.of[IO, Int](0)
        now        <- IO.realTimeInstant
        until   = now.plusSeconds(600).truncatedTo(ChronoUnit.MILLIS)
        request = calls.update(_ + 1) >> IO.raiseError[Unit](AppError.ProviderDeferred(Provider.Finnhub, until, "Retry-After"))
        attempts <- (
          first.run(Provider.Finnhub, ProviderLimits(1, 0, 2))(request).attempt,
          second.run(Provider.Finnhub, ProviderLimits(1, 0, 2))(request).attempt
        ).parTupled
        issued        <- calls.get
        restartedRepo <- ProviderBudgetRepository.make[IO](db)
        restarted     <- ProviderAccess.make[IO](restartedRepo)
        refused       <- restarted.run(Provider.Finnhub, ProviderLimits(1, 0, 2))(calls.update(_ + 1)).attempt
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
        first         <- ProviderAccess.make[IO](original)
        _             <- first.run(Provider.Finnhub, ProviderLimits(60000, 1, 1))(IO.unit)
        _             <- IO.sleep(5.millis)
        restartedRepo <- ProviderBudgetRepository.make[IO](db)
        restarted     <- ProviderAccess.make[IO](restartedRepo)
        now           <- IO.realTimeInstant
        second        <- restarted.run(Provider.Finnhub, ProviderLimits(60000, 1, 1))(IO.unit).attempt
        reset = now.atZone(ZoneOffset.UTC).toLocalDate.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        tomorrow  <- restartedRepo.reserve(Provider.Finnhub, reset, 1L, 1)
        exhausted <- restartedRepo.reserve(Provider.Finnhub, reset.plusSeconds(1), 1L, 1)
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
        _       <- (one.cooldown(Provider.Finnhub, now.plusSeconds(600)), two.cooldown(Provider.Finnhub, now.plusSeconds(60))).parTupled
        refused <- one.reserve(Provider.Finnhub, now, 1L, 100)
        other   <- two.reserve(Provider.TwelveData, now, 1L, 100)
      } yield {
        refused mustBe Some(now.plusSeconds(600))
        other mustBe None
      }
    }

    "retain independent credential budgets when a shared access service is recreated" in withEmbeddedMongoDatabase { db =>
      val limits = ProviderLimits(60000, 1, 1)
      for {
        repository          <- ProviderBudgetRepository.make[IO](db)
        first               <- ProviderAccess.make[IO](repository)
        _                   <- first.run(Provider.TwelveData, limits, Some("credential-one"))(IO.unit)
        restartedRepository <- ProviderBudgetRepository.make[IO](db)
        restarted           <- ProviderAccess.make[IO](restartedRepository)
        firstKey            <- restarted.run(Provider.TwelveData, limits, Some("credential-one"))(IO.unit).attempt
        secondKey           <- restarted.run(Provider.TwelveData, limits, Some("credential-two"))(IO.pure(2))
        otherProvider       <- restarted.run(Provider.Finnhub, limits, Some("credential-one"))(IO.pure(3))
      } yield {
        firstKey.left.toOption.get mustBe a[AppError.ProviderDeferred]
        secondKey mustBe 2
        otherProvider mustBe 3
      }
    }

  }
}
