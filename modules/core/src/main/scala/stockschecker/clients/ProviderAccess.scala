package stockschecker.clients

import stockschecker.domain.{Provider, ProviderDeferralReason}
import cats.effect.{Ref, Temporal}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.domain.errors.AppError
import stockschecker.repositories.ProviderBudgetRepository
import pureconfig.ConfigReader

final case class ProviderLimits(requestsPerMinute: Int, dailyAllowance: Int, maxConcurrent: Int) derives ConfigReader

/** Shared access service with independent quotas, concurrency and credential pauses for each provider account. */
trait ProviderAccess[F[_]]:
  def validateLimits(provider: Provider, limits: ProviderLimits): F[Unit]
  def run[A](provider: Provider, limits: ProviderLimits, account: Option[String] = None)(request: F[A]): F[A]

object ProviderAccess {
  final private case class Account(provider: Provider, credential: Option[String])
  final private case class State(inFlight: Int, disabled: Option[String])
  private object State {
    val empty: State = State(0, None)
  }

  final private class LiveProviderAccess[F[_]](
      state: Ref[F, Map[Account, State]],
      budgets: ProviderBudgetRepository[F]
  )(using F: Temporal[F], C: Clock[F])
      extends ProviderAccess[F] {
    override def validateLimits(provider: Provider, limits: ProviderLimits): F[Unit] =
      F.raiseWhen(limits.requestsPerMinute <= 0 || limits.maxConcurrent <= 0 || limits.dailyAllowance < 0)(
        new IllegalArgumentException(s"Invalid rate limits for ${provider.print}")
      )

    /** Account identifiers must be stable and contain no credentials; omitted accounts retain provider-wide budgets. */
    override def run[A](provider: Provider, limits: ProviderLimits, account: Option[String] = None)(request: F[A]): F[A] = F.uncancelable {
      poll =>
        val key = Account(provider, account)
        for
          _      <- validateLimits(provider, limits)
          now    <- C.now
          result <- state.modify { accounts =>
            val s        = accounts.getOrElse(key, State.empty)
            val deferred = s.disabled
              .map(reason => AppError.ProviderConfiguration(provider, reason): Throwable)
              .orElse(
                Option.when(s.inFlight >= limits.maxConcurrent)(
                  AppError.ProviderDeferred(provider, now.plusSeconds(1), ProviderDeferralReason.ConcurrencyLimit)
                )
              )
            deferred match
              case Some(error) => (accounts, Left(error))
              case None        => (accounts.updated(key, s.copy(inFlight = s.inFlight + 1)), Right(()))
          }
          _     <- F.fromEither(result)
          value <- poll(
            budgets.reserve(
              provider,
              now,
              math.max(1L, (60000L + limits.requestsPerMinute - 1) / limits.requestsPerMinute),
              limits.dailyAllowance,
              account
            )
          )
            .flatMap {
              case None =>
                poll(request).onError {
                  case error: AppError.ProviderDeferred if error.reason == ProviderDeferralReason.Provider429 =>
                    budgets.cooldown(provider, error.retryAt, account)
                  case error: AppError.ProviderConfiguration => update(key)(_.copy(disabled = Some(error.message)))
                }
              case Some(deferred) => F.raiseError[A](AppError.ProviderDeferred(provider, deferred.retryAt, deferred.reason))
            }
            .guarantee(update(key)(s => s.copy(inFlight = math.max(0, s.inFlight - 1))))
        yield value
    }

    private def update(account: Account)(f: State => State): F[Unit] =
      state.update(accounts => accounts.updated(account, f(accounts.getOrElse(account, State.empty))))
  }

  def make[F[_]: {Temporal, Clock}](budgets: ProviderBudgetRepository[F]): F[ProviderAccess[F]] =
    Ref.of[F, Map[Account, State]](Map.empty).map(new LiveProviderAccess(_, budgets))
}
