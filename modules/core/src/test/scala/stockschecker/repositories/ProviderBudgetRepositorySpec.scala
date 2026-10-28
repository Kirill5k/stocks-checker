package stockschecker.repositories

import stockschecker.domain.{Provider, ProviderDeferralReason}
import kirill5k.common.cats.Clock
import cats.effect.{IO, Ref}
import cats.syntax.parallel.*
import mongo4cats.circe.*
import mongo4cats.operations.Filter
import stockschecker.clients.{ProviderAccess, ProviderLimits}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.ProviderBudgetRepository.Deferral
import stockschecker.repositories.entities.ProviderBudgetEntity

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
        request = calls.update(_ + 1) >> IO.raiseError[Unit](
          AppError.ProviderDeferred(Provider.Finnhub, until, ProviderDeferralReason.Provider429)
        )
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
        refused.left.toOption mustBe Some(AppError.ProviderDeferred(Provider.Finnhub, until, ProviderDeferralReason.Provider429))
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
        second.left.toOption mustBe Some(AppError.ProviderDeferred(Provider.Finnhub, reset, ProviderDeferralReason.DailyQuota))
        tomorrow mustBe None
        exhausted mustBe Some(Deferral(reset.plusSeconds(86400), ProviderDeferralReason.DailyQuota))
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
        refused mustBe Some(Deferral(now.plusSeconds(600), ProviderDeferralReason.Provider429))
        other mustBe None
      }
    }

    "distinguish spacing from response cooldowns without consuming rejected requests" in withEmbeddedMongoDatabase { db =>
      val now = Instant.parse("2026-01-01T10:00:00Z")
      for {
        repository       <- ProviderBudgetRepository.make[IO](db)
        collection       <- db.getCollectionWithCodec[ProviderBudgetEntity](ProviderBudgetRepository.CollectionName)
        admitted         <- repository.reserve(Provider.Finnhub, now, 600000L, 100)
        spacing          <- repository.reserve(Provider.Finnhub, now.plusSeconds(1), 600000L, 100)
        _                <- repository.cooldown(Provider.Finnhub, now.plusSeconds(60))
        _                <- repository.cooldown(Provider.Finnhub, now.plusSeconds(30))
        shorterCooldown  <- collection.find(Filter.idEq(Provider.Finnhub.print)).first.map(_.get.budget)
        longerSpacing    <- repository.reserve(Provider.Finnhub, now.plusSeconds(2), 600000L, 100)
        _                <- repository.cooldown(Provider.Finnhub, now.plusSeconds(600))
        restarted        <- ProviderBudgetRepository.make[IO](db)
        responseCooldown <- restarted.reserve(Provider.Finnhub, now.plusSeconds(3), 600000L, 100)
        beforeResume     <- collection.find(Filter.idEq(Provider.Finnhub.print)).first.map(_.get.budget)
        resumed          <- restarted.reserve(Provider.Finnhub, now.plusSeconds(600), 600000L, 100)
        afterResume      <- collection.find(Filter.idEq(Provider.Finnhub.print)).first.map(_.get.budget)
      } yield {
        admitted mustBe None
        spacing mustBe Some(Deferral(now.plusSeconds(600), ProviderDeferralReason.RequestSpacing))
        shorterCooldown.cooldownUntil mustBe Some(now.plusSeconds(60))
        shorterCooldown.nextAllowed mustBe now.plusSeconds(600)
        longerSpacing mustBe spacing
        responseCooldown mustBe Some(Deferral(now.plusSeconds(600), ProviderDeferralReason.Provider429))
        beforeResume.used mustBe 1
        resumed mustBe None
        afterResume.used mustBe 2
        afterResume.cooldownUntil mustBe None
      }
    }

    "report the limiting deadline across daily quota resets and response cooldowns" in withEmbeddedMongoDatabase { db =>
      val now        = Instant.parse("2026-01-01T23:59:50Z")
      val reset      = now.plusSeconds(10)
      val afterReset = reset.plusSeconds(120)
      for {
        repository        <- ProviderBudgetRepository.make[IO](db)
        collection        <- db.getCollectionWithCodec[ProviderBudgetEntity](ProviderBudgetRepository.CollectionName)
        _                 <- repository.reserve(Provider.Finnhub, now, 10000L, 1)
        quotaOverSpacing  <- repository.reserve(Provider.Finnhub, now.plusSeconds(1), 10000L, 1)
        _                 <- repository.cooldown(Provider.Finnhub, reset)
        quotaOverCooldown <- repository.reserve(Provider.Finnhub, now.plusSeconds(2), 10000L, 1)
        _                 <- repository.cooldown(Provider.Finnhub, afterReset)
        cooldownOverQuota <- repository.reserve(Provider.Finnhub, now.plusSeconds(3), 10000L, 1)
        restarted         <- ProviderBudgetRepository.make[IO](db)
        stillCoolingDown  <- restarted.reserve(Provider.Finnhub, reset, 10000L, 1)
        resumed           <- restarted.reserve(Provider.Finnhub, afterReset, 10000L, 1)
        budget            <- collection.find(Filter.idEq(Provider.Finnhub.print)).first.map(_.get.budget)
      } yield {
        quotaOverSpacing mustBe Some(Deferral(reset, ProviderDeferralReason.DailyQuota))
        quotaOverCooldown mustBe quotaOverSpacing
        cooldownOverQuota mustBe Some(Deferral(afterReset, ProviderDeferralReason.Provider429))
        stillCoolingDown mustBe cooldownOverQuota
        resumed mustBe None
        budget.day mustBe reset.atZone(ZoneOffset.UTC).toLocalDate
        budget.used mustBe 1
        budget.cooldownUntil mustBe None
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
