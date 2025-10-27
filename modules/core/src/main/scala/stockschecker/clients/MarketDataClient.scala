package stockschecker.clients

import stockschecker.domain.{CompanyProfile, StockQuote, Ticker}
import fs2.Stream

trait MarketDataClient[F[_]]:
  def getAllTradedStocks: Stream[F, StockQuote]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]]
