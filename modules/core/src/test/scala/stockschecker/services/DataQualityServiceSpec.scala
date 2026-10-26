package stockschecker.services

import cats.effect.IO
import fs2.Stream
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
      val repos      = mock[Repositories[IO]]
      val quality    = mock[DataQualityRepository[IO]]
      val securities = mock[SecurityRepository[IO]]
      val rankings   = mock[InvestmentRankingRepository[IO]]
      val observed   = now.minusSeconds(3600)
      val datasets   = List(
        DatasetAvailability(MSFT, "TwelveData", "daily-prices-all", "failed", now, None, Some("HTTP 503")),
        DatasetAvailability(AAPL, "Finnhub", "financials", "available", observed, Some(observed), None)
      )
      val header = InvestmentRankingRepository.Header(
        "ranking",
        observed,
        InvestmentRecipe(),
        InvestmentRankingSummary(100, 80, Map("missing-financials" -> 20))
      )
      when(repos.dataQuality).thenReturn(quality)
      when(repos.security).thenReturn(securities)
      when(repos.investmentRankings).thenReturn(rankings)
      when(quality.all).thenReturnIO(datasets)
      when(securities.streamAll).thenReturn(Stream.emits(List(AAPLSecurity.copy(exclusionReason = Some("user-excluded")), MSFTSecurity)))
      when(rankings.latestHeader).thenReturnIO(Some(header))
      DataQualityService.make(repos).flatMap(_.report).asserting { report =>
        report.generatedAt mustBe now
        report.rankingAsOf mustBe Some(observed)
        report.datasets.map(_.ticker) mustBe List(AAPL, MSFT)
        report.countsByStatus mustBe Map("available" -> 1, "failed" -> 1)
        report.exclusions mustBe List(ExcludedSecurity(AAPL, "user-excluded", "listed"))
        report.rankingCoverage mustBe Map("eligible" -> 80, "universe" -> 100, "missing-financials" -> 20)
        verify(repos, never).stock
        verify(repos, never).financialHistory
        verify(rankings, never).latest
        succeed
      }
    }

    "report absent ranking coverage explicitly" in {
      val repos      = mock[Repositories[IO]]
      val quality    = mock[DataQualityRepository[IO]]
      val securities = mock[SecurityRepository[IO]]
      val rankings   = mock[InvestmentRankingRepository[IO]]
      when(repos.dataQuality).thenReturn(quality)
      when(repos.security).thenReturn(securities)
      when(repos.investmentRankings).thenReturn(rankings)
      when(quality.all).thenReturnIO(Nil)
      when(securities.streamAll).thenReturn(Stream.empty)
      when(rankings.latestHeader).thenReturnIO(None)
      DataQualityService.make(repos).flatMap(_.report).asserting { report =>
        report.rankingAsOf mustBe None
        report.rankingCoverage mustBe Map.empty
        verify(rankings, never).latest
        succeed
      }
    }
  }
}
