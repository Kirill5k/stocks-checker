package stockschecker.clients

import cats.effect.{Ref, Temporal}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.ProviderBudgetRepository
import pureconfig.ConfigReader

final case class ProviderLimits(requestsPerMinute: Int, dailyAllowance: Int, maxConcurrent: Int) derives ConfigReader

/** Shared budgets own quotas and cooldowns; this instance only owns concurrency and the credential pause. */
final class ProviderAccess[F[_]] private (
    provider: String,
    limits: ProviderLimits,
    state: Ref[F, ProviderAccess.State],
    budgets: ProviderBudgetRepository[F]
)(using F: Temporal[F]) {
  def resume: F[Unit] = state.update(_.copy(disabled = None))

  def run[A](request: F[A]): F[A] = F.uncancelable { poll =>
    for
      now    <- F.realTimeInstant
      result <- state.modify { s =>
        val deferred = s.disabled
          .map(reason => AppError.ProviderConfiguration(provider, reason): Throwable)
          .orElse(
            Option.when(s.inFlight >= limits.maxConcurrent)(
              AppError.ProviderDeferred(provider, now.plusSeconds(1), "provider concurrency limit")
            )
          )
        deferred match
          case Some(error) => (s, Left(error))
          case None        => (s.copy(inFlight = s.inFlight + 1), Right(()))
      }
      _     <- F.fromEither(result)
      value <- poll(budgets.reserve(provider, now, math.max(1L, 60000L / limits.requestsPerMinute), limits.dailyAllowance))
        .flatMap {
          case None =>
            poll(request).onError {
              case error: AppError.ProviderDeferred      => budgets.cooldown(provider, error.retryAt)
              case error: AppError.ProviderConfiguration => state.update(_.copy(disabled = Some(error.message)))
            }
          case Some(until) => F.raiseError[A](AppError.ProviderDeferred(provider, until, "shared provider budget or cooldown"))
        }
        .guarantee(state.update(s => s.copy(inFlight = math.max(0, s.inFlight - 1))))
    yield value
  }
}

object ProviderAccess {
  final private case class State(inFlight: Int, disabled: Option[String])
  def make[F[_]: Temporal](provider: String, limits: ProviderLimits, budgets: ProviderBudgetRepository[F]): F[ProviderAccess[F]] =
    if (limits.requestsPerMinute <= 0 || limits.maxConcurrent <= 0 || limits.dailyAllowance < 0)
      Temporal[F].raiseError(new IllegalArgumentException(s"Invalid rate limits for $provider"))
    else Ref.of[F, State](State(0, None)).map(new ProviderAccess(provider, limits, _, budgets))
}
