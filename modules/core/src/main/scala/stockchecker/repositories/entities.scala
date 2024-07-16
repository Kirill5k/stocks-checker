package stockchecker.repositories

import io.circe.Codec
import stockchecker.domain.{Stock, Ticker}

import java.time.Instant

private[repositories] object entities {

  final case class StockEntity(
      ticker: Ticker,
      name: String,
      price: BigDecimal,
      exchange: String,
      exchangeShortName: String,
      stockType: String,
      lastUpdatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: Stock =
      Stock(
        ticker = ticker,
        name = name,
        price = price,
        exchange = exchange,
        exchangeShortName = exchangeShortName,
        stockType = stockType,
        lastUpdatedAt = lastUpdatedAt
      )

  object StockEntity:
    def from(stock: Stock): StockEntity =
      StockEntity(
        ticker = stock.ticker,
        name = stock.name,
        price = stock.price,
        exchange = stock.exchange,
        exchangeShortName = stock.exchangeShortName,
        stockType = stock.stockType,
        lastUpdatedAt = stock.lastUpdatedAt
      )

}
