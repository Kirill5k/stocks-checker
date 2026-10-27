package stockschecker.utils

import stockschecker.domain.errors.AppError

/** Safe operational messages: never persist provider response bodies or credentials. */
object ProviderFailure:
  def summary(error: Throwable): String = error match
    case e: AppError.ProviderDeferred      => s"Deferred until ${e.retryAt}"
    case _: AppError.DataUnavailable       => "Recent matching provider price data is unavailable"
    case _: AppError.ProviderConfiguration => "Check provider credentials and subscription"
    case e: AppError.HttpClient            => s"HTTP ${e.status}"
    case _: AppError.JsonParsingFailure    => "Invalid provider response"
    case _: AppError.FailedValidation      => "Provider data failed validation"
    case _                                 => error.getClass.getSimpleName
