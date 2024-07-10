package stockchecker

import stockchecker.common.types.StringType

package object domain {

  opaque type Ticker = String
  object Ticker extends StringType[Ticker]

  final case class Stock(
      ticker: Ticker,
      name: String,
      price: BigDecimal,
      exchange: String,
      exchangeShortName: String,
      stockType: String
  )
}
