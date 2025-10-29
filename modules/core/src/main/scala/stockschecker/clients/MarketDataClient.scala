package stockschecker.clients

import cats.Monad
import cats.data.NonEmptyList
import stockschecker.domain.{CompanyProfile, Exchange, PriceCandle, Security, Ticker}
import fs2.Stream
import stockschecker.clients.alphavantage.AlphaVantageClient
import stockschecker.clients.finnhub.FinnhubClient

trait MarketDataClient[F[_]]:
  def getTradedSecurities(exchange: Exchange): Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
  def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]]

final private class LiveMarketDataClient[F[_]](
    private val finnhubClient: FinnhubClient[F],
    private val alphaVantageClient: AlphaVantageClient[F]
) extends MarketDataClient[F] {

  override def getTradedSecurities(exchange: Exchange): Stream[F, Security] =
    finnhubClient.getListedSecurities(exchange)

  override def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
    finnhubClient.getCompanyProfile(ticker)

  override def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    alphaVantageClient.getMonthlyPriceCandles(ticker)
}

object MarketDataClient {
  def make[F[_]: Monad](
      finnhubClient: FinnhubClient[F],
      alphaVantageClient: AlphaVantageClient[F]
  ): F[MarketDataClient[F]] =
    Monad[F].pure(LiveMarketDataClient[F](finnhubClient, alphaVantageClient))
}
