package stockschecker.clients

import cats.effect.Async
import fs2.Stream
import stockschecker.domain.*
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient

trait MarketDataClient[F[_]]:
  def getTradedSecurities(exchange: Exchange): Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
  def getFinancialData(ticker: Ticker): F[Option[FinancialData]]

  /** Fetches daily candles within inclusive date bounds, with the requested corporate-action adjustment and source metadata. */
  def getDailyPriceHistory(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment): F[PriceHistorySnapshot]

final private class LiveMarketDataClient[F[_]](
    finnhub: FinnhubClient[F],
    twelve: TwelveDataClient[F],
    observer: DatasetObserver[F]
) extends MarketDataClient[F] {
  def getTradedSecurities(exchange: Exchange): Stream[F, Security] =
    finnhub.getListedSecurities(exchange)
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
    observer.observe(Provider.Finnhub, Dataset.CompanyProfile, ticker)(finnhub.getCompanyProfile(ticker))(_.nonEmpty)
  def getFinancialData(ticker: Ticker): F[Option[FinancialData]] =
    observer.observe(Provider.Finnhub, Dataset.Financials, ticker)(finnhub.getFinancialData(ticker))(_.nonEmpty)
  def getDailyPriceHistory(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment): F[PriceHistorySnapshot] =
    observer.observe(Provider.TwelveData, Dataset.dailyPrices(adjustment), ticker)(twelve.getDailyPriceHistory(ticker, range, adjustment))(
      _.candles.nonEmpty
    )
}

object MarketDataClient {
  private[clients] def make[F[_]: Async](
      finnhub: FinnhubClient[F],
      twelve: TwelveDataClient[F],
      observer: DatasetObserver[F]
  ): F[MarketDataClient[F]] =
    Async[F].pure(new LiveMarketDataClient(finnhub, twelve, observer))
}
