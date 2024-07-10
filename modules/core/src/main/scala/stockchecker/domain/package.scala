package stockchecker

import stockchecker.common.types.StringType

package object domain {

  opaque type Symbol = String
  object Symbol extends StringType[Symbol]

  final case class Ticker(
      symbol: Symbol,
      name: String,
      price: BigDecimal,
      exchange: String,
      exchangeShortName: String,
      stockType: String
  )
}
