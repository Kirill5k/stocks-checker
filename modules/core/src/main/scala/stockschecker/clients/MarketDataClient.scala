package stockschecker.clients

import stockschecker.domain.{CompanyProfile, Exchange, PriceCandle, Security, Ticker}
import fs2.Stream

trait MarketDataClient[F[_]]:
  def getTradedSecurities(exchange: Exchange): Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
  def getMonthlyPriceCandles(ticker: Ticker): F[List[PriceCandle]]
