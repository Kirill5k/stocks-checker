package stockschecker.clients

import cats.effect.{Ref, Temporal}
import cats.syntax.all.*
import cats.effect.syntax.all.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.{ProviderFailure, Ticker}
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository}
import pureconfig.ConfigReader

final case class ProviderLimits(requestsPerMinute: Int, dailyAllowance: Int, maxConcurrent: Int) derives ConfigReader

/** Shared access service with independent quotas, concurrency and credential pauses for each provider account. */
final class ProviderAccess[F[_]] private (
    state: Ref[F, Map[ProviderAccess.Account, ProviderAccess.State]],
    budgets: ProviderBudgetRepository[F],
    quality: DataQualityRepository[F]
)(using F: Temporal[F]) {
  import ProviderAccess.{Account, State}

  def validateLimits(provider: String, limits: ProviderLimits): F[Unit] =
    F.raiseWhen(limits.requestsPerMinute <= 0 || limits.maxConcurrent <= 0 || limits.dailyAllowance < 0)(
      new IllegalArgumentException(s"Invalid rate limits for $provider")
    )

  /** Account identifiers must be stable and contain no credentials; omitted accounts retain provider-wide budgets. */
  def run[A](provider: String, limits: ProviderLimits, account: Option[String] = None)(request: F[A]): F[A] = F.uncancelable { poll =>
    val key = Account(provider, account)
    for
      _      <- validateLimits(provider, limits)
      now    <- F.realTimeInstant
      result <- state.modify { accounts =>
        val s        = accounts.getOrElse(key, State.empty)
        val deferred = s.disabled
          .map(reason => AppError.ProviderConfiguration(provider, reason): Throwable)
          .orElse(
            Option.when(s.inFlight >= limits.maxConcurrent)(
              AppError.ProviderDeferred(provider, now.plusSeconds(1), "provider concurrency limit")
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
              case error: AppError.ProviderDeferred      => budgets.cooldown(provider, error.retryAt, account)
              case error: AppError.ProviderConfiguration => update(key)(_.copy(disabled = Some(error.message)))
            }
          case Some(until) => F.raiseError[A](AppError.ProviderDeferred(provider, until, "shared provider budget or cooldown"))
        }
        .guarantee(update(key)(s => s.copy(inFlight = math.max(0, s.inFlight - 1))))
    yield value
  }

  def observed[A](ticker: Ticker, provider: String, dataset: String)(request: F[A])(present: A => Boolean): F[A] =
    request.attempt.flatMap {
      case Right(value) => record(ticker, provider, dataset, if (present(value)) "available" else "unavailable", None).as(value)
      case Left(error: AppError.ProviderDeferred) => F.raiseError(error)
      case Left(error)                            =>
        val status = error match
          case _: AppError.ProviderConfiguration         => "configuration-error"
          case e: AppError.HttpClient if e.status == 404 => "unavailable"
          case _                                         => "failed"
        record(ticker, provider, dataset, status, Some(ProviderFailure.summary(error))) >> F.raiseError(error)
    }

  private def update(account: Account)(f: State => State): F[Unit] =
    state.update(accounts => accounts.updated(account, f(accounts.getOrElse(account, State.empty))))

  private def record(ticker: Ticker, provider: String, dataset: String, status: String, failure: Option[String]): F[Unit] =
    F.realTimeInstant.flatMap(now => quality.record(ticker, provider, dataset, status, now, failure))
}

object ProviderAccess {
  final private case class Account(provider: String, credential: Option[String])
  final private case class State(inFlight: Int, disabled: Option[String])
  private object State {
    val empty: State = State(0, None)
  }
  def make[F[_]: Temporal](budgets: ProviderBudgetRepository[F], quality: DataQualityRepository[F]): F[ProviderAccess[F]] =
    Ref.of[F, Map[Account, State]](Map.empty).map(new ProviderAccess(_, budgets, quality))
}
