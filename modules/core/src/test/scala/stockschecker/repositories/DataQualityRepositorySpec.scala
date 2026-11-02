package stockschecker.repositories

import cats.effect.IO
import stockschecker.domain.*
import stockschecker.fixtures.{AAPL, AAPLSecurity, MSFT, MSFTSecurity}

import java.time.Instant

class DataQualityRepositorySpec extends RepositorySpec {
  override def port: Int = 12164
  private val now        = Instant.parse("2026-01-01T12:00:00Z")

  "A DataQualityRepository" should {
    "report complete peer coverage for the latest immutable ranking without counting other exclusions" in withEmbeddedMongoDatabase { db =>
      val sectors  = List(Sector.Technology, Sector.Healthcare, Sector.Industrials)
      val complete = sectors.flatMap { sector =>
        (1 to 19).map { index =>
          val base = InvestmentRepositoryFixtures.entries.head
          base.copy(
            input = base.input.copy(industry = Some(s"  ${sector.print.toUpperCase(java.util.Locale.ROOT)}  "), sector = Some(sector)),
            scorecard = base.scorecard.copy(
              ticker = Ticker(s"${sector.print}$index"),
              eligible = false,
              score = None,
              exclusions = List("insufficient-peer-coverage")
            )
          )
        }
      }
      val excluded = complete.head.copy(scorecard =
        complete.head.scorecard.copy(ticker = Ticker("EXCLUDED"), exclusions = List("missing-financial-snapshot"))
      )
      val entries = complete :+ excluded
      val ranking = InvestmentRanking(InvestmentRankingId("latest-peers"), now, InvestmentRecipe(), InvestmentRankingSummary.from(entries))
      val old     = ranking.copy(id = InvestmentRankingId("old-peers"), createdAt = now.minusSeconds(60))
      for
        repo     <- DataQualityRepository.make[IO](db)
        rankings <- InvestmentRankingRepository.make[IO](db)
        _        <- rankings.save(old, entries)
        _        <- rankings.save(ranking, entries)
        report   <- repo.report(now)
      yield
        val readiness = report.rankingReadiness.get
        readiness.rankingId mustBe ranking.id
        readiness.asOf mustBe now
        readiness.status mustBe RankingReadinessStatus.InsufficientEligible
        readiness.eligibleCount mustBe 0
        readiness.completeCandidateCount mustBe 57
        readiness.completeByIndustry mustBe sectors.map(s => s.print.toLowerCase(java.util.Locale.ROOT) -> 19).toMap
        readiness.completeBySector mustBe sectors.map(_ -> 19).toMap
    }

    List(false, true).foreach { hasExcludedInput =>
      s"retain the latest ranking header when it has ${if hasExcludedInput then "only excluded" else "no"} inputs" in
        withEmbeddedMongoDatabase { db =>
          val base     = InvestmentRepositoryFixtures.entries.head
          val oldEntry = base.copy(input = base.input.copy(industry = Some("Software"), sector = Some(Sector.Technology)))
          val excluded =
            oldEntry.copy(scorecard = oldEntry.scorecard.copy(eligible = false, exclusions = List("missing-financial-snapshot")))
          val latestEntries = if hasExcludedInput then List(excluded) else Nil
          val latest        = InvestmentRanking(
            InvestmentRankingId("latest-empty-peers"),
            now,
            InvestmentRecipe(),
            InvestmentRankingSummary.from(latestEntries)
          )
          val older = latest.copy(
            id = InvestmentRankingId("older-with-peers"),
            createdAt = now.minusSeconds(60),
            summary = InvestmentRankingSummary.from(List(oldEntry))
          )
          for
            repo     <- DataQualityRepository.make[IO](db)
            rankings <- InvestmentRankingRepository.make[IO](db)
            _        <- rankings.save(older, List(oldEntry))
            _        <- rankings.save(latest, latestEntries)
            report   <- repo.report(now)
          yield
            report.rankingAsOf mustBe Some(now)
            val readiness = report.rankingReadiness.get
            readiness.rankingId mustBe latest.id
            readiness.eligibleCount mustBe 0
            readiness.completeCandidateCount mustBe 0
            readiness.completeByIndustry mustBe Map.empty
            readiness.completeBySector mustBe Map.empty
        }
    }

    "group peer counts before embedding ranking inputs larger than MongoDB's document limit" in withEmbeddedMongoDatabase { db =>
      val payload = "x" * (1024 * 1024)
      val base    = InvestmentRepositoryFixtures.entries.head
      val entries = (1 to 20).map { index =>
        base.copy(
          input = base.input.copy(industry = Some("Software"), sector = Some(Sector.Technology)),
          scorecard = base.scorecard.copy(ticker = Ticker(s"LARGE$index"), weaknesses = List(payload))
        )
      }.toList
      val ranking = InvestmentRanking(
        InvestmentRankingId("large-inputs"),
        now,
        InvestmentRecipe(),
        InvestmentRankingSummary.from(entries)
      )
      for
        repo     <- DataQualityRepository.make[IO](db)
        rankings <- InvestmentRankingRepository.make[IO](db)
        _        <- rankings.save(ranking, entries)
        report   <- repo.report(now)
      yield
        val readiness = report.rankingReadiness.get
        readiness.rankingId mustBe ranking.id
        readiness.completeCandidateCount mustBe 20
        readiness.completeByIndustry mustBe Map("software" -> 20)
        readiness.completeBySector mustBe Map(Sector.Technology -> 20)
    }

    "separate available provider prices from stale or missing persisted analytics" in {
      import mongo4cats.bson.Document
      import mongo4cats.bson.syntax.*
      val data = Map(
        PriceAnalyticsRepository.CollectionName -> List(
          Document(
            "_id"                := AAPL.value,
            "updatedAt"          := now,
            "performanceSummary" := Document("latestPrice" := BigDecimal(100), "latestPriceDate" := "2025-12-24")
          )
        )
      )
      withEmbeddedMongoDatabase(data) { db =>
        for
          repo    <- DataQualityRepository.make[IO](db)
          _       <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesUnadjusted, DatasetStatus.Available, now, None)
          _       <- repo.record(MSFT, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Available, now, None)
          report  <- repo.report(now)
          relaxed <- repo.report(now, InvestmentRecipe(maxPriceAgeDays = 8))
        yield
          report.countsByStatus mustBe Map(DatasetStatus.Available -> 2)
          report.priceAnalyticsReadiness.map(_.ticker) mustBe List(AAPL, MSFT)
          report.priceAnalyticsReadiness.head.usable mustBe false
          report.priceAnalyticsReadiness.head.reason mustBe Some("stale-price")
          report.priceAnalyticsReadiness.head.calculatedAt mustBe Some(now)
          report.priceAnalyticsReadiness(1) mustBe PriceAnalyticsReadiness.missing(MSFT)
          relaxed.priceAnalyticsReadiness.head.usable mustBe true
      }
    }

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
        yield report.copy(rankingReadiness = None, priceAnalyticsReadiness = Nil) mustBe DataQualityReport(
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
        yield report.copy(rankingReadiness = None, priceAnalyticsReadiness = Nil) mustBe DataQualityReport(
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
      yield report.copy(rankingReadiness = None, priceAnalyticsReadiness = Nil) mustBe DataQualityReport(
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
        result <- repo.report(now).map(_.datasets)
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
        failed <- repo.report(now).map(_.datasets)
        _      <- repo.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplitsAndDividends, DatasetStatus.Available, recoveredAt, None)
        recovered <- repo.report(now).map(_.datasets)
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
        rows <- repo.report(now).map(_.datasets)
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
