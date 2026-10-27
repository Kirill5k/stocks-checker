package stockschecker.repositories

import cats.effect.IO
import stockschecker.domain.*
import stockschecker.fixtures.{AAPL, AAPLSecurity, MSFT, MSFTSecurity}

import java.time.Instant

class DataQualityRepositorySpec extends RepositorySpec {
  override def port: Int = 12164
  private val now        = Instant.parse("2026-01-01T12:00:00Z")

  "A DataQualityRepository" should {
    "return an empty report when none of the source collections contain data" in withEmbeddedMongoDatabase { db =>
      for
        repo   <- DataQualityRepository.make[IO](db)
        report <- repo.report(now)
      yield report mustBe DataQualityReport(now, Nil, Nil, Map.empty, Map.empty, None)
    }

    "aggregate sorted availability, status counts, exclusions, and the latest published ranking" in {
      val observedAt = now.minusSeconds(3600)
      val seedData   = Map(
        SecurityRepository.CollectionName -> List(
          toDoc(MSFTSecurity.copy(exclusionReason = Some("user-excluded"), listingStatus = ListingStatus.Delisted)),
          toDoc(AAPLSecurity.copy(exclusionReason = Some("unsupported-security"))),
          toDoc(AAPLSecurity.copy(ticker = Ticker("INCLUDED")))
        )
      )
      val latest = InvestmentRanking(
        InvestmentRankingId("latest"),
        observedAt,
        InvestmentRecipe(),
        InvestmentRankingSummary(100, 80, Map("missing-financials" -> 20, "eligible" -> -1, "universe" -> -1))
      )
      val earlier = latest.copy(
        id = InvestmentRankingId("earlier"),
        createdAt = observedAt.minusSeconds(60),
        summary = InvestmentRankingSummary(50, 30, Map("old-reason" -> 20))
      )
      withEmbeddedMongoDatabase(seedData) { db =>
        for
          repo     <- DataQualityRepository.make[IO](db)
          rankings <- InvestmentRankingRepository.make[IO](db)
          _        <- rankings.save(latest, Nil)
          _        <- rankings.save(earlier, Nil)
          _        <- repo.record(
            MSFT,
            Provider.TwelveData,
            Dataset.DailyPricesSplitsAndDividends,
            DatasetStatus.Failed,
            observedAt,
            Some("HTTP 503")
          )
          _      <- repo.record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, DatasetStatus.Available, observedAt, None)
          _      <- repo.record(AAPL, Provider.Finnhub, Dataset.Financials, DatasetStatus.Available, observedAt, None)
          report <- repo.report(now)
        yield report mustBe DataQualityReport(
          now,
          List(
            DatasetAvailability(AAPL, Provider.Finnhub, Dataset.Financials, DatasetStatus.Available, observedAt, Some(observedAt), None),
            DatasetAvailability(
              AAPL,
              Provider.Finnhub,
              Dataset.CompanyProfile,
              DatasetStatus.Available,
              observedAt,
              Some(observedAt),
              None
            ),
            DatasetAvailability(
              MSFT,
              Provider.TwelveData,
              Dataset.DailyPricesSplitsAndDividends,
              DatasetStatus.Failed,
              observedAt,
              None,
              Some("HTTP 503")
            )
          ),
          List(
            ExcludedSecurity(AAPL, "unsupported-security", ListingStatus.Listed),
            ExcludedSecurity(MSFT, "user-excluded", ListingStatus.Delisted)
          ),
          Map(DatasetStatus.Available -> 2, DatasetStatus.Failed -> 1),
          Map("eligible"              -> 80, "universe"          -> 100, "missing-financials" -> 20),
          Some(observedAt)
        )
      }
    }

    "include exclusions and ranking coverage even before any dataset has been observed" in {
      val observedAt = now.minusSeconds(60)
      val seedData   = Map(SecurityRepository.CollectionName -> List(toDoc(AAPLSecurity.copy(exclusionReason = Some("user-excluded")))))
      val ranking    = InvestmentRanking(
        InvestmentRankingId("published"),
        observedAt,
        InvestmentRecipe(),
        InvestmentRankingSummary(10, 8, Map("missing-financials" -> 2))
      )
      withEmbeddedMongoDatabase(seedData) { db =>
        for
          repo     <- DataQualityRepository.make[IO](db)
          rankings <- InvestmentRankingRepository.make[IO](db)
          _        <- rankings.save(ranking, Nil)
          report   <- repo.report(now)
        yield report mustBe DataQualityReport(
          now,
          Nil,
          List(ExcludedSecurity(AAPL, "user-excluded", ListingStatus.Listed)),
          Map.empty,
          Map("eligible" -> 8, "universe" -> 10, "missing-financials" -> 2),
          Some(observedAt)
        )
      }
    }

    "report availability without manufacturing coverage when no ranking has been published" in withEmbeddedMongoDatabase { db =>
      for
        repo   <- DataQualityRepository.make[IO](db)
        _      <- repo.record(AAPL, Provider.Finnhub, Dataset.Financials, DatasetStatus.Unavailable, now, None)
        report <- repo.report(now)
      yield report mustBe DataQualityReport(
        now,
        List(DatasetAvailability(AAPL, Provider.Finnhub, Dataset.Financials, DatasetStatus.Unavailable, now, None, None)),
        Nil,
        Map(DatasetStatus.Unavailable -> 1),
        Map.empty,
        None
      )
    }

    "record an unavailable dataset before it has ever succeeded" in withEmbeddedMongoDatabase { db =>
      for
        repo <- DataQualityRepository.make[IO](db)
        _ <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Unavailable, now, Some("HTTP 404"))
        result <- repo.all
      yield result mustBe List(
        DatasetAvailability(
          AAPL,
          Provider.TwelveData,
          Dataset.DailyPricesSplitsAndDividends,
          DatasetStatus.Unavailable,
          now,
          None,
          Some("HTTP 404")
        )
      )
    }

    "upsert repeated attempts, retain the last success on failure, and clear failures after recovery" in withEmbeddedMongoDatabase { db =>
      val failedAt    = now.plusSeconds(60)
      val recoveredAt = now.plusSeconds(120)
      for
        repo <- DataQualityRepository.make[IO](db)
        _    <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Available, now, None)
        _ <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Failed, failedAt, Some("HTTP 503"))
        failed <- repo.all
        _      <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Available, recoveredAt, None)
        recovered <- repo.all
      yield
        failed mustBe List(
          DatasetAvailability(
            AAPL,
            Provider.TwelveData,
            Dataset.DailyPricesSplitsAndDividends,
            DatasetStatus.Failed,
            failedAt,
            Some(now),
            Some("HTTP 503")
          )
        )
        recovered mustBe List(
          DatasetAvailability(
            AAPL,
            Provider.TwelveData,
            Dataset.DailyPricesSplitsAndDividends,
            DatasetStatus.Available,
            recoveredAt,
            Some(recoveredAt),
            None
          )
        )
    }

    "keep availability independent for each ticker, provider, and dataset" in withEmbeddedMongoDatabase { db =>
      for
        repo <- DataQualityRepository.make[IO](db)
        _    <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Available, now, None)
        _    <- repo.record(AAPL, Provider.Finnhub, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Unavailable, now, Some("HTTP 404"))
        _    <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplits, DatasetStatus.Failed, now, Some("HTTP 503"))
        _    <- repo.record(MSFT, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Available, now, None)
        rows <- repo.all
      yield rows.toSet mustBe Set(
        DatasetAvailability(
          AAPL,
          Provider.TwelveData,
          Dataset.DailyPricesSplitsAndDividends,
          DatasetStatus.Available,
          now,
          Some(now),
          None
        ),
        DatasetAvailability(
          AAPL,
          Provider.Finnhub,
          Dataset.DailyPricesSplitsAndDividends,
          DatasetStatus.Unavailable,
          now,
          None,
          Some("HTTP 404")
        ),
        DatasetAvailability(AAPL, Provider.TwelveData, Dataset.DailyPricesSplits, DatasetStatus.Failed, now, None, Some("HTTP 503")),
        DatasetAvailability(MSFT, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Available, now, Some(now), None)
      )
    }
  }
}
