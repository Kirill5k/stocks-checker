package stockschecker.clients

import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.{Provider, ProviderDeferralReason}
import stockschecker.domain.errors.AppError

import java.time.Instant

class ProviderErrorsSpec extends IOWordSpec {
  private val now = Instant.parse("2026-10-01T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  "ProviderErrors" should {
    "apply the same retry, authentication and HTTP failure policy to both providers" in
      Provider.values.toList
        .traverse { provider =>
          for
            deferred     <- ProviderErrors.fromStatus[IO](provider, 429, Some("120"), "sanitized context")
            unauthorized <- ProviderErrors.fromStatus[IO](provider, 401, message = "sanitized context")
            forbidden    <- ProviderErrors.fromStatus[IO](provider, 403, message = "sanitized context")
            unavailable  <- ProviderErrors.fromStatus[IO](provider, 503, message = "sanitized context")
          yield
            deferred mustBe AppError.ProviderDeferred(
              provider,
              now.plusSeconds(120),
              ProviderDeferralReason.Provider429,
              Some("sanitized context")
            )
            deferred.message must include("sanitized context")
            List(unauthorized, forbidden).foreach { error =>
              error mustBe a[AppError.ProviderConfiguration]
              error.message must include("sanitized context")
              error.message must include("check credentials and subscription")
            }
            unavailable mustBe AppError.HttpClient(provider.print, 503, "sanitized context")
        }
        .asserting(_ => succeed)

    "omit response details from operational summaries while preserving endpoint context on exceptions" in
      ProviderErrors
        .fromStatus[IO](Provider.TwelveData, 429, Some("60"), "time_series for AAPL: provider response with api_key=secret-value")
        .asserting { error =>
          error mustBe AppError.ProviderDeferred(
            Provider.TwelveData,
            now.plusSeconds(60),
            ProviderDeferralReason.Provider429,
            Some("time_series for AAPL: provider response with api_key=secret-value")
          )
          error.message must include("time_series for AAPL")
          val summary = AppError.safeSummary(error)
          summary must include(Provider.TwelveData.print)
          summary must include(ProviderDeferralReason.Provider429.print)
          summary must not include "provider response"
          summary must not include "api_key"
          summary must not include "secret-value"
          summary must not include "time_series for AAPL"
          succeed
        }
  }
}
