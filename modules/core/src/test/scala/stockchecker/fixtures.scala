package stockchecker

import stockchecker.domain.{Stock, Ticker}

import java.time.Instant
import java.time.temporal.ChronoUnit

object fixtures {

  val ts = Instant.now.truncatedTo(ChronoUnit.MILLIS)

  val AAPL = Ticker("AAPL")

  val AAPLSock = Stock(
    ticker = AAPL,
    name = "Apple Inc.",
    price = BigDecimal(234.4),
    exchange = "NASDAQ Global Select",
    exchangeShortName = "NASDAQ",
    stockType = "stock",
    lastUpdatedAt = ts
  )
}
