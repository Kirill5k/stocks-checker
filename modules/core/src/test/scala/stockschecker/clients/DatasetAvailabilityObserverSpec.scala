package stockschecker.clients

import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.AAPL
import stockschecker.repositories.DataQualityRepository

import java.time.Instant

class DatasetAvailabilityObserverSpec extends IOWordSpec {
  private val now = Instant.parse("2026-10-01T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  "A DatasetAvailabilityObserver" should {
    "record present and absent results using the injected clock without changing the results" in {
      val quality = mock[DataQualityRepository[IO]]
      when(quality.record(AAPL, "test", "prices", "available", now, None)).thenReturnUnit
      when(quality.record(AAPL, "test", "prices", "unavailable", now, None)).thenReturnUnit
      (for
        observer <- DatasetAvailabilityObserver.make[IO](quality)
        present  <- observer.observed(AAPL, "test", "prices")(IO.pure(Option(1)))(_.nonEmpty)
        absent   <- observer.observed(AAPL, "test", "prices")(IO.pure(Option.empty[Int]))(_.nonEmpty)
      yield (present, absent)).asserting { result =>
        result mustBe (Some(1), None)
        verify(quality).record(AAPL, "test", "prices", "available", now, None)
        verify(quality).record(AAPL, "test", "prices", "unavailable", now, None)
        verifyNoMoreInteractions(quality)
        succeed
      }
    }

    "record failed requests once and propagate their original errors" in {
      val cases = List(
        (AppError.HttpClient("test", 404, "missing"), "unavailable", "HTTP 404"),
        (AppError.ProviderConfiguration("test", "unauthorized"), "configuration-error", "Check provider credentials and subscription"),
        (new java.io.IOException("offline"), "failed", "IOException")
      )
      cases
        .traverse { case (error, status, summary) =>
          val quality = mock[DataQualityRepository[IO]]
          when(quality.record(AAPL, "test", "prices", status, now, Some(summary))).thenReturnUnit
          (for
            observer <- DatasetAvailabilityObserver.make[IO](quality)
            result   <- observer.observed(AAPL, "test", "prices")(IO.raiseError[Option[Int]](error))(_.nonEmpty).attempt
          yield result).map { result =>
            result mustBe Left(error)
            verify(quality).record(AAPL, "test", "prices", status, now, Some(summary))
            verifyNoMoreInteractions(quality)
            succeed
          }
        }
        .asserting(_ => succeed)
    }

    "leave dataset availability unchanged when the logical request is deferred" in {
      val quality = mock[DataQualityRepository[IO]]
      val error   = AppError.ProviderDeferred("test", now.plusSeconds(60), "cooldown")
      (for
        observer <- DatasetAvailabilityObserver.make[IO](quality)
        result   <- observer.observed(AAPL, "test", "prices")(IO.raiseError[Option[Int]](error))(_.nonEmpty).attempt
      yield result).asserting { result =>
        result mustBe Left(error)
        verifyNoInteractions(quality)
        succeed
      }
    }
  }
}
