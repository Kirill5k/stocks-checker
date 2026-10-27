package stockschecker.clients

import stockschecker.domain.Provider
import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import fs2.Stream
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient
import stockschecker.domain.{
  Dataset,
  DatasetStatus,
  DateRange,
  Exchange,
  FinancialData,
  FinancialSnapshot,
  PriceAdjustment,
  PriceHistorySnapshot
}
import stockschecker.fixtures.*
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository}

import java.time.{Instant, LocalDate}

class MarketDataClientSpec extends IOWordSpec {
  given Clock[IO]      = Clock.mock(ts)
  private val limits   = ProviderLimits(60000, 0, 1)
  private val from     = LocalDate.parse("2025-01-01")
  private val through  = LocalDate.parse("2025-12-01")
  private val range    = DateRange(from, through)
  private val snapshot = PriceHistorySnapshot(
    "prices",
    AAPL,
    AAPLPriceCandles.toList,
    Provider.TwelveData,
    "USD",
    PriceAdjustment.Splits,
    ts,
    range
  )
  private val financials = FinancialData(
    AAPLFinancialMetrics,
    FinancialSnapshot(
      "financials",
      AAPL,
      Provider.Finnhub,
      ts,
      Map.empty,
      Nil,
      Nil,
      "hash",
      "mapping",
      Nil
    ),
    "{}"
  )

  private def budgets: ProviderBudgetRepository[IO] = {
    val repository = mock[ProviderBudgetRepository[IO]]
    when(repository.reserve(any[Provider], any[Instant], any[Long], any[Int], any[Option[String]])).thenReturnIO(None)
    repository
  }

  "MarketDataClient" should {
    "delegate every request and result without reserving provider access a second time" in {
      val finnhub = mock[FinnhubClient[IO]]
      val twelve  = mock[TwelveDataClient[IO]]
      val budget  = budgets
      val quality = mock[DataQualityRepository[IO]]
      when(quality.record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, DatasetStatus.Available, ts, None)).thenReturnUnit
      when(quality.record(AAPL, Provider.Finnhub, Dataset.Financials, DatasetStatus.Available, ts, None)).thenReturnUnit
      when(quality.record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplits, DatasetStatus.Available, ts, None)).thenReturnUnit
      (for
        access   <- ProviderAccess.make[IO](budget)
        observer <- DatasetObserver.make[IO](quality)
        _ = when(finnhub.getListedSecurities(Exchange.NASDAQ)).thenReturn(
          Stream.eval(access.run(Provider.Finnhub, limits)(IO.pure(List(AAPLSecurity, MSFTSecurity)))).flatMap(Stream.emits)
        )
        _ = when(finnhub.getCompanyProfile(AAPL)).thenReturn(access.run(Provider.Finnhub, limits)(IO.pure(Some(AAPLCompanyProfile))))
        _ = when(finnhub.getFinancialData(AAPL)).thenReturn(access.run(Provider.Finnhub, limits)(IO.pure(Some(financials))))
        _ = when(twelve.getDailyPriceHistory(AAPL, range, PriceAdjustment.Splits))
          .thenReturn(access.run(Provider.TwelveData, limits)(IO.pure(snapshot)))
        client     <- MarketDataClient.make[IO](finnhub, twelve, observer)
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
        verify(budget, times(3)).reserve(
          org.mockito.ArgumentMatchers.eq(Provider.Finnhub),
          any[Instant],
          any[Long],
          any[Int],
          any[Option[String]]
        )
        verify(budget).reserve(org.mockito.ArgumentMatchers.eq(Provider.TwelveData), any[Instant], any[Long], any[Int], any[Option[String]])
        verify(quality).record(AAPL, Provider.Finnhub, Dataset.CompanyProfile, DatasetStatus.Available, ts, None)
        verify(quality).record(AAPL, Provider.Finnhub, Dataset.Financials, DatasetStatus.Available, ts, None)
        verify(quality).record(AAPL, Provider.TwelveData, Dataset.DailyPricesSplits, DatasetStatus.Available, ts, None)
        verifyNoMoreInteractions(budget, finnhub, twelve, quality)
        succeed
      }
    }

    "record each daily adjustment under its stable dataset name, including empty results" in {
      val cases = List(
        PriceAdjustment.Unadjusted         -> Dataset.DailyPricesUnadjusted,
        PriceAdjustment.Splits             -> Dataset.DailyPricesSplits,
        PriceAdjustment.SplitsAndDividends -> Dataset.DailyPricesSplitsAndDividends
      )
      cases
        .traverse { case (adjustment, dataset) =>
          val finnhub = mock[FinnhubClient[IO]]
          val twelve  = mock[TwelveDataClient[IO]]
          val quality = mock[DataQualityRepository[IO]]
          val empty   = snapshot.copy(adjustment = adjustment, candles = Nil)
          when(twelve.getDailyPriceHistory(AAPL, range, adjustment)).thenReturnIO(empty)
          when(quality.record(AAPL, Provider.TwelveData, dataset, DatasetStatus.Unavailable, ts, None)).thenReturnUnit
          (for
            observer <- DatasetObserver.make[IO](quality)
            client   <- MarketDataClient.make[IO](finnhub, twelve, observer)
            result   <- client.getDailyPriceHistory(AAPL, range, adjustment)
          yield result).map { result =>
            result mustBe empty
            verify(quality).record(AAPL, Provider.TwelveData, dataset, DatasetStatus.Unavailable, ts, None)
            verifyNoInteractions(finnhub)
            succeed
          }
        }
        .asserting(_ => succeed)
    }

  }
}
