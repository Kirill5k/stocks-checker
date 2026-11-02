package stockschecker.clients

import cats.Monad
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.{Provider, ProviderDeferralReason, Ticker}
import stockschecker.domain.errors.AppError

/** Shared provider status mapping. Callers supply only sanitized diagnostic messages. */
object ProviderErrors:
  def fromStatus[F[_]](
      provider: Provider,
      status: Int,
      retryAfter: Option[String] = None,
      message: String = "Provider request failed",
      ticker: Option[Ticker] = None
  )(using F: Monad[F], C: Clock[F]): F[AppError] = status match
    case 429 =>
      C.now.map { now =>
        AppError.ProviderDeferred(
          provider,
          RetryAfter.deadline(retryAfter, now),
          ProviderDeferralReason.Provider429,
          Some(message)
        )
      }
    case 401 => F.pure(AppError.ProviderConfiguration(provider, s"$message; HTTP $status; check credentials and subscription"))
    case 403 if ticker.nonEmpty => F.pure(AppError.DataUnavailable(ticker.get, message))
    case _                      => F.pure(AppError.HttpClient(provider.print, status, message))
