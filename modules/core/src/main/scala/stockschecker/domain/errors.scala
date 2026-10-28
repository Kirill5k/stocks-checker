package stockschecker.domain

object errors {
  sealed trait AppError extends Throwable:
    def message: String
    override def getMessage: String = message

  object AppError:
    /** Safe operational messages: never persist provider response bodies or credentials. */
    def safeSummary(error: Throwable): String = error match
      case e: ProviderDeferred      => s"Deferred: provider=${e.provider.print} reason=${e.reason.print} retryAt=${e.retryAt}"
      case _: DataUnavailable       => "Recent matching provider price data is unavailable"
      case _: ProviderConfiguration => "Check provider credentials and subscription"
      case e: HttpClient            => s"HTTP ${e.status}"
      case _: JsonParsingFailure    => "Invalid provider response"
      case _: FailedValidation      => "Provider data failed validation"
      case _                        => error.getClass.getSimpleName

    sealed trait Unauth        extends AppError
    sealed trait NotFound      extends AppError
    sealed trait Conflict      extends AppError
    sealed trait BadReq        extends AppError
    sealed trait Forbidden     extends AppError
    sealed trait Unprocessable extends AppError

    final case class FailedValidation(message: String) extends Unprocessable
    final case class Critical(message: String)         extends AppError

    final case class ProviderDeferred(
        provider: Provider,
        retryAt: java.time.Instant,
        reason: ProviderDeferralReason,
        context: Option[String] = None
    ) extends AppError:
      override def message: String = safeSummary(this) + context.fold("")(value => s": $value")

    final case class ProviderConfiguration(provider: Provider, message: String) extends AppError

    /** Missing market observations use bounded job retries, unlike provider quota deferrals. */
    final case class DataUnavailable(ticker: Ticker, reason: String) extends AppError:
      override def message: String = s"Data unavailable for $ticker: $reason"

    final case class HttpClient(name: String, status: Int, error: String) extends AppError:
      override def message: String = s"$name HTTP $status - $error"

    final case class EntityDoesNotExist(entityName: String, id: String) extends NotFound:
      override val message: String = s"$entityName with id $id does not exist"

    final case class SecurityNotFound(ticker: Ticker) extends NotFound:
      override val message: String = s"Could not find security for $ticker"

    final case class CompanyProfileNotFound(ticker: Ticker) extends NotFound:
      override val message: String = s"Could not find company profile for $ticker"

    final case class PriceAnalyticsNotFound(ticker: Ticker) extends NotFound:
      override val message: String = s"Could not find price analytics for $ticker"

    final case class FinancialMetricsNotFound(ticker: Ticker) extends NotFound:
      override val message: String = s"Could not find financial metrics for $ticker"

    final case class JsonParsingFailure(original: String, error: String) extends AppError:
      override val message: String = s"Failed to parse json response: $error\n$original"
}
