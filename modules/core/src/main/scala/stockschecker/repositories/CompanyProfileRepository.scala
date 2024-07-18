package stockschecker.repositories

import stockschecker.domain.{CompanyProfile, Ticker}

trait CompanyProfileRepository[F[_]]:
  def save(cp: CompanyProfile): F[Unit]
  def find(ticker: Ticker): F[Option[CompanyProfile]]
