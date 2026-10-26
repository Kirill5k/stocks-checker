package stockschecker.clients

import cats.effect.{Ref, Temporal}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.ProviderBudgetRepository
import java.time.{Instant, LocalDate, ZoneOffset}
import pureconfig.ConfigReader

final case class ProviderLimits(requestsPerMinute: Int = 60, dailyAllowance: Int = 0, maxConcurrent: Int = 2) derives ConfigReader

/** One instance per provider/account. Deferrals release job workers instead of sleeping in them. */
final class ProviderAccess[F[_]] private (
    provider: String,
    limits: ProviderLimits,
    state: Ref[F, ProviderAccess.State],
    budgets: Option[ProviderBudgetRepository[F]]
)(using F: Temporal[F]) {
  import ProviderAccess.State

  def run[A](request: F[A]): F[A] = F.uncancelable { poll =>
    for
      now <- F.realTimeInstant
      result <- state.modify { old =>
        val day = now.atZone(ZoneOffset.UTC).toLocalDate
        val s = if (old.day == day) old else old.copy(day = day, used = 0)
        val dailyReset = day.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        val deferred =
          s.disabled.map(reason => AppError.ProviderConfiguration(provider, reason): Throwable)
            .orElse(Option.when(budgets.isEmpty && limits.dailyAllowance > 0 && s.used >= limits.dailyAllowance)(AppError.ProviderDeferred(provider, dailyReset, "daily allowance exhausted")))
            .orElse(Option.when(budgets.isEmpty && now.isBefore(s.nextAllowed))(AppError.ProviderDeferred(provider, s.nextAllowed, "provider cooldown or request quota")))
            .orElse(Option.when(s.inFlight >= limits.maxConcurrent)(AppError.ProviderDeferred(provider, now.plusSeconds(1), "provider concurrency limit")))
        deferred match
          case Some(error) => (s, Left(error))
          case None =>
            val admitted = if (budgets.isEmpty) State(day, s.used + 1, s.inFlight + 1, now.plusMillis(math.max(1L, 60000L / limits.requestsPerMinute)), None)
              else s.copy(inFlight = s.inFlight + 1)
            (admitted, Right(()))
      }
      _ <- F.fromEither(result)
      value <- (poll(reserveShared(now)) >> poll(request).onError {
        case error: AppError.ProviderDeferred => cooldown(error.retryAt)
        case error: AppError.ProviderConfiguration => state.update(_.copy(disabled = Some(error.message)))
        case error: AppError.HttpClient if error.status == 401 || error.status == 403 => state.update(_.copy(disabled = Some(s"HTTP ${error.status}; check provider credentials/entitlements")))
        case error: AppError.HttpClient if error.status == 429 => F.realTimeInstant.flatMap(at => cooldown(at.plusSeconds(60)))
      }.adaptError {
        case error: AppError.HttpClient if error.status == 401 || error.status == 403 =>
          AppError.ProviderConfiguration(provider, s"HTTP ${error.status}; check provider credentials/entitlements")
      }).guarantee(state.update(s => s.copy(inFlight = math.max(0, s.inFlight - 1))))
    yield value
  }

  private def reserveShared(now: Instant): F[Unit] = budgets.traverse_ { repository =>
    repository.reserve(provider, now, math.max(1L, 60000L / limits.requestsPerMinute), limits.dailyAllowance).flatMap {
      case None => F.unit
      case Some(until) => F.raiseError(AppError.ProviderDeferred(provider, until, "shared provider budget or cooldown"))
    }
  }

  private def cooldown(until: Instant): F[Unit] =
    state.update(s => s.copy(nextAllowed = if (s.nextAllowed.isAfter(until)) s.nextAllowed else until)) >>
      budgets.traverse_(_.cooldown(provider, until))
}

object ProviderAccess {
  private final case class State(day: LocalDate, used: Int, inFlight: Int, nextAllowed: Instant, disabled: Option[String])
  def make[F[_]: Temporal](provider: String, limits: ProviderLimits, budgets: Option[ProviderBudgetRepository[F]] = None): F[ProviderAccess[F]] =
    if (limits.requestsPerMinute <= 0 || limits.maxConcurrent <= 0 || limits.dailyAllowance < 0)
      Temporal[F].raiseError(new IllegalArgumentException(s"Invalid rate limits for $provider"))
    else Ref.of[F, State](State(LocalDate.MIN, 0, 0, Instant.EPOCH, None)).map(new ProviderAccess(provider, limits, _, budgets))
}
