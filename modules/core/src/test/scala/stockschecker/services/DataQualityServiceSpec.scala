package stockschecker.services

import cats.effect.{IO, Ref}
import fs2.Stream
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.MarketDataClient
import stockschecker.common.config.InvestmentConfig
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.*

import java.time.Instant

class DataQualityServiceSpec extends IOWordSpec {
  private val now    = Instant.parse("2026-10-26T12:00:00Z")
  private val config = InvestmentConfig(true, true, false, 7, 14, 180, 550)
  given Clock[IO]    = Clock.mock(now)

  "DataQualityService" should {
    "resume the provider before releasing its paused jobs and normalize the name" in {
      val repos  = mock[Repositories[IO]]
      val jobs   = mock[JobRepository[IO]]
      val client = mock[MarketDataClient[IO]]
      when(repos.jobs).thenReturn(jobs)
      (for
        calls <- Ref.of[IO, List[String]](Nil)
        _ = when(client.resumeProvider("TwelveData")).thenReturn(calls.update(_ :+ "provider"))
        _ = when(jobs.resumePaused("TwelveData", now)).thenReturn(calls.update(_ :+ "jobs"))
        service  <- DataQualityService.make(repos, client, config)
        _        <- service.resumeProvider("tWeLvEdAtA")
        recorded <- calls.get
      yield recorded).asserting(_ mustBe List("provider", "jobs"))
    }

    "reject unknown provider names without changing any state" in {
      val repos  = mock[Repositories[IO]]
      val client = mock[MarketDataClient[IO]]
      DataQualityService.make(repos, client, config).flatMap(_.resumeProvider("unknown")).attempt.asserting { result =>
        result.left.toOption.get mustBe a[AppError.FailedValidation]
        verifyNoInteractions(repos, client)
        succeed
      }
    }

    "leave paused jobs alone if resetting the client fails" in {
      val repos   = mock[Repositories[IO]]
      val client  = mock[MarketDataClient[IO]]
      val failure = new IllegalStateException("provider reset failed")
      when(client.resumeProvider("Finnhub")).thenRaiseError(failure)
      DataQualityService.make(repos, client, config).flatMap(_.resumeProvider("Finnhub")).attempt.asserting { result =>
        result mustBe Left(failure)
        verify(repos, never).jobs
        succeed
      }
    }

    "read stored ranking coverage from the header without rescoring or loading financial histories" in {
      val repos      = mock[Repositories[IO]]
      val client     = mock[MarketDataClient[IO]]
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
      DataQualityService.make(repos, client, config).flatMap(_.report).asserting { report =>
        report.generatedAt mustBe now
        report.rankingAsOf mustBe Some(observed)
        report.datasets.map(_.ticker) mustBe List(AAPL, MSFT)
        report.countsByStatus mustBe Map("available" -> 1, "failed" -> 1)
        report.exclusions mustBe List(ExcludedSecurity(AAPL, "user-excluded", "listed"))
        report.rankingCoverage mustBe Map("eligible" -> 80, "universe" -> 100, "missing-financials" -> 20)
        report.rankingEnabled mustBe true
        report.coverageAudited mustBe true
        verify(repos, never).stock
        verify(repos, never).financialHistory
        verify(rankings, never).latest
        verifyNoInteractions(client)
        succeed
      }
    }

    "report absent ranking coverage explicitly" in {
      val repos      = mock[Repositories[IO]]
      val client     = mock[MarketDataClient[IO]]
      val quality    = mock[DataQualityRepository[IO]]
      val securities = mock[SecurityRepository[IO]]
      val rankings   = mock[InvestmentRankingRepository[IO]]
      when(repos.dataQuality).thenReturn(quality)
      when(repos.security).thenReturn(securities)
      when(repos.investmentRankings).thenReturn(rankings)
      when(quality.all).thenReturnIO(Nil)
      when(securities.streamAll).thenReturn(Stream.empty)
      when(rankings.latestHeader).thenReturnIO(None)
      DataQualityService.make(repos, client, config).flatMap(_.report).asserting { report =>
        report.rankingAsOf mustBe None
        report.rankingCoverage mustBe Map.empty
        verify(rankings, never).latest
        succeed
      }
    }
  }
}
