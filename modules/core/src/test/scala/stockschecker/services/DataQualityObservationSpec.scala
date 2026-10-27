package stockschecker.services

import stockschecker.domain.DatasetStatus
import stockschecker.domain.Provider
import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.AAPL
import stockschecker.repositories.DataQualityRepository

import java.time.Instant

class DataQualityObservationSpec extends IOWordSpec {
  private val now = Instant.parse("2026-10-01T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  "DataQualityService observation" should {
    "record present and absent results using the injected clock without changing the results" in {
      val quality = mock[DataQualityRepository[IO]]
      when(quality.record(AAPL, Provider.Finnhub, "prices", DatasetStatus.Available, now, None)).thenReturnUnit
      when(quality.record(AAPL, Provider.Finnhub, "prices", DatasetStatus.Unavailable, now, None)).thenReturnUnit
      (for
        observer <- DataQualityService.make[IO](quality, mock[stockschecker.repositories.SecurityRepository[IO]], mock[InvestmentRankingService[IO]])
        present  <- observer.observe(AAPL, Provider.Finnhub, "prices")(IO.pure(Option(1)))(_.nonEmpty)
        absent   <- observer.observe(AAPL, Provider.Finnhub, "prices")(IO.pure(Option.empty[Int]))(_.nonEmpty)
      yield (present, absent)).asserting { result =>
        result mustBe (Some(1), None)
        verify(quality).record(AAPL, Provider.Finnhub, "prices", DatasetStatus.Available, now, None)
        verify(quality).record(AAPL, Provider.Finnhub, "prices", DatasetStatus.Unavailable, now, None)
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
          when(quality.record(AAPL, Provider.Finnhub, "prices", status, now, Some(summary))).thenReturnUnit
          (for
            observer <- DataQualityService.make[IO](quality, mock[stockschecker.repositories.SecurityRepository[IO]], mock[InvestmentRankingService[IO]])
            result   <- observer.observe(AAPL, Provider.Finnhub, "prices")(IO.raiseError[Option[Int]](error))(_.nonEmpty).attempt
          yield result).map { result =>
            result mustBe Left(error)
            verify(quality).record(AAPL, Provider.Finnhub, "prices", status, now, Some(summary))
            verifyNoMoreInteractions(quality)
            succeed
          }
        }
        .asserting(_ => succeed)
    }

    "leave dataset availability unchanged when the logical request is deferred" in {
      val quality = mock[DataQualityRepository[IO]]
      val error   = AppError.ProviderDeferred(Provider.Finnhub, now.plusSeconds(60), "cooldown")
      (for
        observer <- DataQualityService.make[IO](quality, mock[stockschecker.repositories.SecurityRepository[IO]], mock[InvestmentRankingService[IO]])
        result   <- observer.observe(AAPL, Provider.Finnhub, "prices")(IO.raiseError[Option[Int]](error))(_.nonEmpty).attempt
      yield result).asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(quality)
        succeed
      }
    }
  }
}
