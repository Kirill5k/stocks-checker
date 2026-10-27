package stockschecker.clients

import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.Provider
import stockschecker.domain.errors.AppError

import java.time.Instant

class ProviderErrorsSpec extends IOWordSpec {
  private val now = Instant.parse("2026-10-01T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  "ProviderErrors" should {
    "apply the same retry, authentication and HTTP failure policy to both providers" in
      Provider.values.toList.traverse { provider =>
        for
          deferred <- ProviderErrors.fromStatus[IO](provider, 429, Some("120"), "response body")
          unauthorized <- ProviderErrors.fromStatus[IO](provider, 401, message = "response body")
          forbidden <- ProviderErrors.fromStatus[IO](provider, 403, message = "response body")
          unavailable <- ProviderErrors.fromStatus[IO](provider, 503, message = "sanitized context")
        yield
          deferred mustBe AppError.ProviderDeferred(provider, now.plusSeconds(120), "rate-limited")
          List(unauthorized, forbidden).foreach { error =>
            error mustBe a[AppError.ProviderConfiguration]
            error.message must not include "response body"
          }
          unavailable mustBe AppError.HttpClient(provider.print, 503, "sanitized context")
      }.asserting(_ => succeed)
  }
}
