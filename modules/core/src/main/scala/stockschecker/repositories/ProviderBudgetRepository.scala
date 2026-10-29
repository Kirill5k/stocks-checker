package stockschecker.repositories

import stockschecker.domain.{Provider, ProviderDeferralReason}
import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.repositories.entities.{ProviderBudget, ProviderBudgetEntity}
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.UpdateOptions
import mongo4cats.operations.{Filter, Update}

import java.time.{Instant, LocalDate, ZoneOffset}

/** One stable key per configured provider account. Usage and response cooldowns survive process restarts. */
trait ProviderBudgetRepository[F[_]] {

  /** None reserves one outbound request; Some(deferral) refuses it without consuming the budget. */
  def reserve(
      provider: Provider,
      now: Instant,
      spacingMillis: Long,
      dailyAllowance: Int,
      account: Option[String] = None
  ): F[Option[ProviderBudgetRepository.Deferral]]
  def cooldown(provider: Provider, until: Instant, account: Option[String] = None): F[Unit]
}

final private class LiveProviderBudgetRepository[F[_]: Async](
    collection: MongoCollection[F, ProviderBudgetEntity]
) extends ProviderBudgetRepository[F] {
  import ProviderBudgetRepository.{Deferral, Field}

  private val emptyBudget = ProviderBudget(LocalDate.ofEpochDay(0L), 0L, Instant.EPOCH, 0L)

  private def budgetId(provider: Provider, account: Option[String]): String =
    account.fold(provider.print)(id => s"${provider.print}:$id")

  private def read(provider: Provider, account: Option[String]): F[ProviderBudget] =
    collection.find(Filter.idEq(budgetId(provider, account))).first.flatMap {
      case Some(record) => record.budget.pure[F]
      case None         =>
        collection
          .updateOne(
            Filter.idEq(budgetId(provider, account)),
            Update.setOnInsert(Field.Budget, emptyBudget),
            UpdateOptions(upsert = true)
          )
          .void >> read(provider, account)
    }

  private def replace(account: String, before: ProviderBudget, after: ProviderBudget): F[Boolean] =
    collection
      .updateOne(
        Filter.idEq(account) && Filter.eq(Field.Version, before.version),
        Update.set(Field.Budget, after.copy(version = before.version + 1))
      )
      .map(_.getMatchedCount == 1)

  def reserve(
      provider: Provider,
      now: Instant,
      spacingMillis: Long,
      dailyAllowance: Int,
      account: Option[String]
  ): F[Option[Deferral]] =
    read(provider, account).flatMap { before =>
      val day           = now.atZone(ZoneOffset.UTC).toLocalDate
      val current       = if (before.day == day) before else before.copy(day = day, used = 0L)
      val reset         = day.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
      val quotaUntil    = Option.when(dailyAllowance > 0 && current.used >= dailyAllowance.toLong)(reset)
      val cooldownUntil = current.cooldownUntil.filter(now.isBefore)
      val spacingUntil  = Option.when(now.isBefore(current.nextAllowed))(current.nextAllowed)
      val deferredUntil = (quotaUntil.toList ++ cooldownUntil.toList ++ spacingUntil.toList).maxOption
      deferredUntil match {
        case Some(until) =>
          val reason =
            if quotaUntil.contains(until) then ProviderDeferralReason.DailyQuota
            else if cooldownUntil.contains(until) then ProviderDeferralReason.Provider429
            else ProviderDeferralReason.RequestSpacing
          Some(Deferral(until, reason)).pure[F]
        case None =>
          replace(
            budgetId(provider, account),
            before,
            current.copy(used = current.used + 1L, nextAllowed = now.plusMillis(spacingMillis.max(1L)), cooldownUntil = None)
          )
            .flatMap(updated => if (updated) none[Deferral].pure[F] else reserve(provider, now, spacingMillis, dailyAllowance, account))
      }
    }

  def cooldown(provider: Provider, until: Instant, account: Option[String]): F[Unit] =
    read(provider, account).flatMap { before =>
      val nextAllowed   = if until.isAfter(before.nextAllowed) then until else before.nextAllowed
      val cooldownUntil = (before.cooldownUntil.toList :+ until).max
      val after         = before.copy(nextAllowed = nextAllowed, cooldownUntil = Some(cooldownUntil))
      if (before == after) Async[F].unit
      else
        replace(budgetId(provider, account), before, after)
          .flatMap(updated => if (updated) Async[F].unit else cooldown(provider, until, account))
    }
}

object ProviderBudgetRepository extends MongoJsonCodecs {
  val CollectionName = "provider-budgets"

  final case class Deferral(retryAt: Instant, reason: ProviderDeferralReason)

  object Field:
    val Budget  = "budget"
    val Version = "budget.version"

  def make[F[_]: Async](db: MongoDatabase[F]): F[ProviderBudgetRepository[F]] =
    db.getCollectionWithCodec[ProviderBudgetEntity](CollectionName)
      .map(collection => LiveProviderBudgetRepository(collection.withAddedCodec[ProviderBudget]))

}
