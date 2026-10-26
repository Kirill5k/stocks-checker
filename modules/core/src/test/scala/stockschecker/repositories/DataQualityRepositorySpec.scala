package stockschecker.repositories

import cats.effect.IO
import stockschecker.domain.DatasetAvailability
import stockschecker.fixtures.{AAPL, MSFT}

import java.time.Instant

class DataQualityRepositorySpec extends RepositorySpec {
  override def port: Int = 12164
  private val now       = Instant.parse("2026-01-01T12:00:00Z")

  "A DataQualityRepository" should {
    "record an unavailable dataset before it has ever succeeded" in withEmbeddedMongoDatabase { db =>
      for
        repo   <- DataQualityRepository.make[IO](db)
        _      <- repo.record(AAPL, "TwelveData", "daily-prices-all", "unavailable", now, Some("HTTP 404"))
        result <- repo.all
      yield result mustBe List(
        DatasetAvailability(AAPL, "TwelveData", "daily-prices-all", "unavailable", now, None, Some("HTTP 404"))
      )
    }

    "upsert repeated attempts, retain the last success on failure, and clear failures after recovery" in withEmbeddedMongoDatabase {
      db =>
        val failedAt    = now.plusSeconds(60)
        val recoveredAt = now.plusSeconds(120)
        for
          repo      <- DataQualityRepository.make[IO](db)
          _         <- repo.record(AAPL, "TwelveData", "daily-prices-all", "available", now, None)
          _         <- repo.record(AAPL, "TwelveData", "daily-prices-all", "failed", failedAt, Some("HTTP 503"))
          failed    <- repo.all
          _         <- repo.record(AAPL, "TwelveData", "daily-prices-all", "available", recoveredAt, None)
          recovered <- repo.all
        yield
          failed mustBe List(
            DatasetAvailability(AAPL, "TwelveData", "daily-prices-all", "failed", failedAt, Some(now), Some("HTTP 503"))
          )
          recovered mustBe List(
            DatasetAvailability(AAPL, "TwelveData", "daily-prices-all", "available", recoveredAt, Some(recoveredAt), None)
          )
    }

    "keep availability independent for each ticker, provider, and dataset" in withEmbeddedMongoDatabase { db =>
      for
        repo <- DataQualityRepository.make[IO](db)
        _    <- repo.record(AAPL, "TwelveData", "daily-prices-all", "available", now, None)
        _    <- repo.record(AAPL, "Finnhub", "daily-prices-all", "unavailable", now, Some("HTTP 404"))
        _    <- repo.record(AAPL, "TwelveData", "daily-prices-splits", "failed", now, Some("HTTP 503"))
        _    <- repo.record(MSFT, "TwelveData", "daily-prices-all", "available", now, None)
        rows <- repo.all
      yield rows.toSet mustBe Set(
        DatasetAvailability(AAPL, "TwelveData", "daily-prices-all", "available", now, Some(now), None),
        DatasetAvailability(AAPL, "Finnhub", "daily-prices-all", "unavailable", now, None, Some("HTTP 404")),
        DatasetAvailability(AAPL, "TwelveData", "daily-prices-splits", "failed", now, None, Some("HTTP 503")),
        DatasetAvailability(MSFT, "TwelveData", "daily-prices-all", "available", now, Some(now), None)
      )
    }
  }
}
