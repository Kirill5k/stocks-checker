package stockchecker

import stockchecker.domain.{Stock, Ticker}

import java.time.Instant

object fixtures {

  val ts = Instant.now

  val aapl = Ticker("AAPL")

  val aaplStock = Stock(
    ticker = aapl,
    name = "Apple Inc.",
    price = BigDecimal(234.4),
    exchange = "NASDAQ Global Select",
    exchangeShortName = "NASDAQ",
    stockType = "stock",
    lastUpdatedAt = ts
  )
}
