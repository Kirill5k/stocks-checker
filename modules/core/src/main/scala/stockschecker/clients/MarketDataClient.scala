package stockschecker.clients

import stockschecker.domain.{CompanyProfile, Stock, Ticker}
import fs2.Stream

trait MarketDataClient[F[_]]:
  def getAllTradedStocks: Stream[F, Stock]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
