package stockschecker.repositories

import stockschecker.domain.DatasetStatus

import stockschecker.domain.Provider
import cats.effect.IO
import stockschecker.domain.DatasetAvailability
import stockschecker.fixtures.{AAPL, MSFT}

import java.time.Instant

class DataQualityRepositorySpec extends RepositorySpec {
  override def port: Int = 12164
  private val now        = Instant.parse("2026-01-01T12:00:00Z")

  "A DataQualityRepository" should {
    "record an unavailable dataset before it has ever succeeded" in withEmbeddedMongoDatabase { db =>
      for
        repo   <- DataQualityRepository.make[IO](db)
        _      <- repo.record(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Unavailable, now, Some("HTTP 404"))
        result <- repo.all
      yield result mustBe List(
        DatasetAvailability(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Unavailable, now, None, Some("HTTP 404"))
      )
    }

    "upsert repeated attempts, retain the last success on failure, and clear failures after recovery" in withEmbeddedMongoDatabase { db =>
      val failedAt    = now.plusSeconds(60)
      val recoveredAt = now.plusSeconds(120)
      for
        repo      <- DataQualityRepository.make[IO](db)
        _         <- repo.record(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Available, now, None)
        _         <- repo.record(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Failed, failedAt, Some("HTTP 503"))
        failed    <- repo.all
        _         <- repo.record(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Available, recoveredAt, None)
        recovered <- repo.all
      yield
        failed mustBe List(
          DatasetAvailability(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Failed, failedAt, Some(now), Some("HTTP 503"))
        )
        recovered mustBe List(
          DatasetAvailability(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Available, recoveredAt, Some(recoveredAt), None)
        )
    }

    "keep availability independent for each ticker, provider, and dataset" in withEmbeddedMongoDatabase { db =>
      for
        repo <- DataQualityRepository.make[IO](db)
        _    <- repo.record(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Available, now, None)
        _    <- repo.record(AAPL, Provider.Finnhub, "daily-prices-all", DatasetStatus.Unavailable, now, Some("HTTP 404"))
        _    <- repo.record(AAPL, Provider.TwelveData, "daily-prices-splits", DatasetStatus.Failed, now, Some("HTTP 503"))
        _    <- repo.record(MSFT, Provider.TwelveData, "daily-prices-all", DatasetStatus.Available, now, None)
        rows <- repo.all
      yield rows.toSet mustBe Set(
        DatasetAvailability(AAPL, Provider.TwelveData, "daily-prices-all", DatasetStatus.Available, now, Some(now), None),
        DatasetAvailability(AAPL, Provider.Finnhub, "daily-prices-all", DatasetStatus.Unavailable, now, None, Some("HTTP 404")),
        DatasetAvailability(AAPL, Provider.TwelveData, "daily-prices-splits", DatasetStatus.Failed, now, None, Some("HTTP 503")),
        DatasetAvailability(MSFT, Provider.TwelveData, "daily-prices-all", DatasetStatus.Available, now, Some(now), None)
      )
    }
  }
}
