package stockschecker.clients

import stockschecker.domain.{CompanyProfile, Security, Ticker}
import fs2.Stream

trait MarketDataClient[F[_]]:
  def getAllTradedSecurities: Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
