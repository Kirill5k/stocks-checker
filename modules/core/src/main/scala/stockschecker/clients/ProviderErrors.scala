package stockschecker.clients

import cats.Monad
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.Provider
import stockschecker.domain.errors.AppError

/** Shared provider status mapping. Callers supply only sanitized diagnostic messages. */
object ProviderErrors:
  def fromStatus[F[_]](
      provider: Provider,
      status: Int,
      retryAfter: Option[String] = None,
      message: String = "Provider request failed"
  )(using F: Monad[F], C: Clock[F]): F[AppError] = status match
    case 429 => C.now.map(now => AppError.ProviderDeferred(provider, RetryAfter.deadline(retryAfter, now), "rate-limited"))
    case 401 | 403 => F.pure(AppError.ProviderConfiguration(provider, s"HTTP $status; check credentials and subscription"))
    case _ => F.pure(AppError.HttpClient(provider.print, status, message))
