package stockschecker

import stockschecker.domain.{Stock, Ticker}

import java.time.Instant
import java.time.temporal.ChronoUnit

object fixtures {

  val ts = Instant.now.truncatedTo(ChronoUnit.MILLIS)

  val AAPL = Ticker("AAPL")
  val MSFT = Ticker("MSFT")

  val AAPLStock = Stock(
    ticker = AAPL,
    name = "Apple Inc.",
    price = BigDecimal(234.4),
    exchange = "NASDAQ Global Select",
    exchangeShortName = "NASDAQ",
    stockType = "stock",
    lastUpdatedAt = ts
  )

  val MSFTStock = Stock(
    ticker = MSFT,
    name = "Microsoft Corporation",
    price = BigDecimal(449.52),
    exchange = "NASDAQ Global Select",
    exchangeShortName = "NASDAQ",
    stockType = "stock",
    lastUpdatedAt = ts
  )
}
