package stockschecker.clients

import stockschecker.domain.{Dataset, DatasetStatus}
import stockschecker.domain.{Provider, ProviderDeferralReason}
import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.AAPL
import stockschecker.repositories.DataQualityRepository

import java.time.Instant

class DatasetObserverSpec extends IOWordSpec {
  private val now = Instant.parse("2026-10-01T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  "DatasetObserver" should {
    "record present and absent results using the injected clock without changing the results" in {
      val quality = mock[DataQualityRepository[IO]]
      when(quality.record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, DatasetStatus.Available, now, None)).thenReturnUnit
      when(quality.record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, DatasetStatus.Unavailable, now, None)).thenReturnUnit
      (for
        observer <- DatasetObserver.make[IO](quality)
        present  <- observer.observe(Provider.Finnhub, Dataset.CompanyProfile, AAPL)(IO.pure(Option(1)))(_.nonEmpty)
        absent   <- observer.observe(Provider.Finnhub, Dataset.CompanyProfile, AAPL)(IO.pure(Option.empty[Int]))(_.nonEmpty)
      yield (present, absent)).asserting { result =>
        result mustBe (Some(1), None)
        verify(quality).record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, DatasetStatus.Available, now, None)
        verify(quality).record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, DatasetStatus.Unavailable, now, None)
        verifyNoMoreInteractions(quality)
        succeed
      }
    }

    "record failed requests once and propagate their original errors" in {
      val cases = List(
        (AppError.HttpClient(Provider.Finnhub.print, 404, "missing"), DatasetStatus.Unavailable, "HTTP 404"),
        (
          AppError.ProviderConfiguration(Provider.Finnhub, "unauthorized"),
          DatasetStatus.ConfigurationError,
          "Check provider credentials and subscription"
        ),
        (new java.io.IOException("offline"), DatasetStatus.Failed, "IOException")
      )
      cases
        .traverse { case (error, status, summary) =>
          val quality = mock[DataQualityRepository[IO]]
          when(quality.record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, status, now, Some(summary))).thenReturnUnit
          (for
            observer <- DatasetObserver.make[IO](quality)
            result   <- observer
              .observe(Provider.Finnhub, Dataset.CompanyProfile, AAPL)(IO.raiseError[Option[Int]](error))(_.nonEmpty)
              .attempt
          yield result).map { result =>
            result mustBe Left(error)
            verify(quality).record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, status, now, Some(summary))
            verifyNoMoreInteractions(quality)
            succeed
          }
        }
        .asserting(_ => succeed)
    }

    "leave dataset availability unchanged when the logical request is deferred" in {
      val quality = mock[DataQualityRepository[IO]]
      val error   = AppError.ProviderDeferred(Provider.Finnhub, now.plusSeconds(60), ProviderDeferralReason.Provider429)
      (for
        observer <- DatasetObserver.make[IO](quality)
        result   <- observer.observe(Provider.Finnhub, Dataset.CompanyProfile, AAPL)(IO.raiseError[Option[Int]](error))(_.nonEmpty).attempt
      yield result).asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(quality)
        succeed
      }
    }
  }
}
