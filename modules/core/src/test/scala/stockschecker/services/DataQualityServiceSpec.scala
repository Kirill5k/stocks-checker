package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.repositories.*

import java.time.Instant

class DataQualityServiceSpec extends IOWordSpec {
  private val now = Instant.parse("2026-10-26T12:00:00Z")
  given Clock[IO] = Clock.mock(now)

  "DataQualityService" should {
    "read stored ranking coverage from the header without rescoring or loading financial histories" in {
      val quality    = mock[DataQualityRepository[IO]]
      val securities = mock[SecurityRepository[IO]]
      val rankings   = mock[InvestmentRankingService[IO]]
      val observed   = now.minusSeconds(3600)
      val datasets   = List(
        DatasetAvailability(MSFT, Provider.TwelveData, "daily-prices-all", DatasetStatus.Failed, now, None, Some("HTTP 503")),
        DatasetAvailability(AAPL, Provider.Finnhub, "financials", DatasetStatus.Available, observed, Some(observed), None)
      )
      val header = InvestmentRanking(
        InvestmentRankingId("ranking"),
        observed,
        InvestmentRecipe(),
        InvestmentRankingSummary(100, 80, Map("missing-financials" -> 20))
      )
      when(quality.all).thenReturnIO(datasets)
      when(securities.findExcluded).thenReturnIO(List(AAPLSecurity.copy(exclusionReason = Some("user-excluded"))))
      when(rankings.latestAvailable).thenReturnIO(Some(header))
      DataQualityService.make(quality, securities, rankings).flatMap(_.report).asserting { report =>
        report.generatedAt mustBe now
        report.rankingAsOf mustBe Some(observed)
        report.datasets.map(_.ticker) mustBe List(AAPL, MSFT)
        report.countsByStatus mustBe Map(DatasetStatus.Available -> 1, DatasetStatus.Failed -> 1)
        report.exclusions mustBe List(ExcludedSecurity(AAPL, "user-excluded", ListingStatus.Listed))
        report.rankingCoverage mustBe Map("eligible" -> 80, "universe" -> 100, "missing-financials" -> 20)
        verify(rankings, never).latest
        verify(securities, never).streamAll
        succeed
      }
    }

    "report absent ranking coverage explicitly" in {
      val quality    = mock[DataQualityRepository[IO]]
      val securities = mock[SecurityRepository[IO]]
      val rankings   = mock[InvestmentRankingService[IO]]
      when(quality.all).thenReturnIO(Nil)
      when(securities.findExcluded).thenReturnIO(Nil)
      when(rankings.latestAvailable).thenReturnIO(None)
      DataQualityService.make(quality, securities, rankings).flatMap(_.report).asserting { report =>
        report.rankingAsOf mustBe None
        report.rankingCoverage mustBe Map.empty
        report.exclusions mustBe Nil
        verify(rankings, never).latest
        verify(securities, never).streamAll
        succeed
      }
    }
  }
}
