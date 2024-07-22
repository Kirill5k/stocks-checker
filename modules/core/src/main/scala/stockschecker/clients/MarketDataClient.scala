package stockschecker.clients

import stockschecker.domain.{CompanyProfile, Stock, Ticker}

trait MarketDataClient[F[_]]:
  def getAllTradedStocks: F[List[Stock]]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
