package stockchecker.clients

import stockchecker.domain.Ticker

trait FinancialModelingPrepClient[F[_]]:
  def allTradedStocks: F[List[Ticker]]