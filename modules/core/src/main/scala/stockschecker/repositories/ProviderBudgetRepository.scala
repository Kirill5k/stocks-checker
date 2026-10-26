package stockschecker.repositories

import cats.effect.Async
import cats.syntax.all.*
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.UpdateOptions
import mongo4cats.operations.{Filter, Update}

import java.time.{Instant, LocalDate, ZoneOffset}

/** One stable key per configured provider account. Usage and response cooldowns survive process restarts. */
trait ProviderBudgetRepository[F[_]] {

  /** None reserves one outbound request; Some(deadline) refuses it without consuming the budget. */
  def reserve(account: String, now: Instant, spacingMillis: Long, dailyAllowance: Int): F[Option[Instant]]
  def cooldown(account: String, until: Instant): F[Unit]
}

object ProviderBudgetRepository extends MongoJsonCodecs {
  val CollectionName = "provider-budgets"
  final private case class Budget(day: LocalDate, used: Long, nextAllowed: Instant, version: Long) derives Codec.AsObject
  final private case class Entity(_id: String, budget: Budget) derives Codec.AsObject

  def make[F[_]: Async](db: MongoDatabase[F]): F[ProviderBudgetRepository[F]] =
    db.getCollectionWithCodec[Entity](CollectionName).map(collection => new Live(collection.withAddedCodec[Budget]))

  final private class Live[F[_]: Async](collection: MongoCollection[F, Entity]) extends ProviderBudgetRepository[F] {
    private def read(account: String): F[Budget] =
      collection.find(Filter.idEq(account)).first.flatMap {
        case Some(record) => record.budget.pure[F]
        case None         =>
          val initial = Budget(LocalDate.ofEpochDay(0L), 0L, Instant.EPOCH, 0L)
          collection.updateOne(Filter.idEq(account), Update.setOnInsert("budget", initial), UpdateOptions(upsert = true)).void >> read(
            account
          )
      }

    private def replace(account: String, before: Budget, after: Budget): F[Boolean] =
      collection
        .updateOne(
          Filter.idEq(account) && Filter.eq("budget.version", before.version),
          Update.set("budget", after.copy(version = before.version + 1))
        )
        .map(_.getMatchedCount == 1)

    def reserve(account: String, now: Instant, spacingMillis: Long, dailyAllowance: Int): F[Option[Instant]] =
      read(account).flatMap { before =>
        val day           = now.atZone(ZoneOffset.UTC).toLocalDate
        val current       = if (before.day == day) before else before.copy(day = day, used = 0L)
        val reset         = day.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        val quotaUntil    = Option.when(dailyAllowance > 0 && current.used >= dailyAllowance.toLong)(reset)
        val deferredUntil = (quotaUntil.toList ++ Option.when(now.isBefore(current.nextAllowed))(current.nextAllowed)).maxOption
        deferredUntil match {
          case Some(until) => Some(until).pure[F]
          case None        =>
            replace(account, before, current.copy(used = current.used + 1L, nextAllowed = now.plusMillis(spacingMillis.max(1L))))
              .flatMap(updated => if (updated) none[Instant].pure[F] else reserve(account, now, spacingMillis, dailyAllowance))
        }
      }

    def cooldown(account: String, until: Instant): F[Unit] =
      read(account).flatMap { before =>
        if (!until.isAfter(before.nextAllowed)) Async[F].unit
        else
          replace(account, before, before.copy(nextAllowed = until)).flatMap(updated =>
            if (updated) Async[F].unit else cooldown(account, until)
          )
      }
  }
}
