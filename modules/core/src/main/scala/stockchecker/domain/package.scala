package stockchecker

import stockchecker.common.types.StringType

import java.time.{Instant, LocalDate}

package object domain {

  opaque type Ticker = String
  object Ticker extends StringType[Ticker]

  final case class Stock(
      ticker: Ticker,
      name: String,
      price: BigDecimal,
      exchange: String,
      exchangeShortName: String,
      stockType: String,
      lastUpdatedAt: Instant
  )

  final case class CompanyProfile(
      ticker: Ticker,
      name: String,
      country: String,
      sector: String,
      industry: String,
      description: String,
      website: String,
      ipoDate: LocalDate,
      currency: String,
      price: BigDecimal,
      mktCap: Long,
      volAvg: Long,
      isEtf: Boolean,
      isActivelyTrading: Boolean,
      isFund: Boolean,
      isAdr: Boolean
  )
}
