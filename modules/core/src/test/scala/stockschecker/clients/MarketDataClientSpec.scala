package stockschecker.clients

import cats.effect.IO
import fs2.Stream
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient
import stockschecker.domain.{DateRange, Exchange, FinancialData, FinancialSnapshot, InvestmentScorer, PriceAdjustment, PriceHistorySnapshot}
import stockschecker.fixtures.*
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository}

import java.time.{Instant, LocalDate}

class MarketDataClientSpec extends IOWordSpec {
  private val limits   = ProviderLimits(60000, 0, 1)
  private val from     = LocalDate.parse("2025-01-01")
  private val through  = LocalDate.parse("2025-12-01")
  private val range    = DateRange(from, through)
  private val snapshot = PriceHistorySnapshot(
    "prices",
    AAPL,
    AAPLPriceCandles.toList,
    "TwelveData",
    "USD",
    PriceAdjustment.Splits,
    ts,
    range
  )
  private val financials = FinancialData(
    AAPLFinancialMetrics,
    FinancialSnapshot("financials", AAPL, "Finnhub", ts, Map.empty, Nil, Nil, "{}", "hash", InvestmentScorer.MappingVersion, Nil)
  )

  private def budgets: ProviderBudgetRepository[IO] = {
    val repository = mock[ProviderBudgetRepository[IO]]
    when(repository.reserve(anyString, any[Instant], any[Long], any[Int], any[Option[String]])).thenReturnIO(None)
    repository
  }

  "MarketDataClient" should {
    "delegate every request and result without reserving provider access a second time" in {
      val finnhub = mock[FinnhubClient[IO]]
      val twelve  = mock[TwelveDataClient[IO]]
      val budget  = budgets
      val quality = mock[DataQualityRepository[IO]]
      (for
        access <- ProviderAccess.make[IO](budget, quality)
        _ = when(finnhub.getListedSecurities(Exchange.NASDAQ)).thenReturn(
          Stream.eval(access.run("Finnhub", limits)(IO.pure(List(AAPLSecurity, MSFTSecurity)))).flatMap(Stream.emits)
        )
        _ = when(finnhub.getCompanyProfile(AAPL)).thenReturn(access.run("Finnhub", limits)(IO.pure(Some(AAPLCompanyProfile))))
        _ = when(finnhub.getFinancialData(AAPL)).thenReturn(access.run("Finnhub", limits)(IO.pure(Some(financials))))
        _ = when(twelve.getDailyPriceHistory(AAPL, range, PriceAdjustment.Splits))
          .thenReturn(access.run("TwelveData", limits)(IO.pure(snapshot)))
        client     <- MarketDataClient.make[IO](finnhub, twelve)
        securities <- client.getTradedSecurities(Exchange.NASDAQ).compile.toList
        profile    <- client.getCompanyProfile(AAPL)
        metrics    <- client.getFinancialData(AAPL)
        prices     <- client.getDailyPriceHistory(AAPL, range, PriceAdjustment.Splits)
      yield (securities, profile, metrics, prices)).asserting { case (securities, profile, metrics, prices) =>
        securities mustBe List(AAPLSecurity, MSFTSecurity)
        profile mustBe Some(AAPLCompanyProfile)
        metrics mustBe Some(financials)
        prices mustBe snapshot
        verify(finnhub).getListedSecurities(Exchange.NASDAQ)
        verify(finnhub).getCompanyProfile(AAPL)
        verify(finnhub).getFinancialData(AAPL)
        verify(twelve).getDailyPriceHistory(AAPL, range, PriceAdjustment.Splits)
        verify(budget, times(3)).reserve(org.mockito.ArgumentMatchers.eq("Finnhub"), any[Instant], any[Long], any[Int], any[Option[String]])
        verify(budget).reserve(org.mockito.ArgumentMatchers.eq("TwelveData"), any[Instant], any[Long], any[Int], any[Option[String]])
        verifyNoMoreInteractions(budget, finnhub, twelve)
        verifyNoInteractions(quality)
        succeed
      }
    }

  }
}
