package stockschecker

import io.circe.Codec as CirceCodec
import stockschecker.common.types.{EnumType, StringType}
import sttp.tapir.{Codec, DecodeResult, Schema}

import java.time.{Instant, LocalDate}

package object domain {

  opaque type Ticker = String
  object Ticker extends StringType[Ticker] {
    inline given Codec.PlainCodec[Ticker] = Codec.string.mapDecode[Ticker](s => DecodeResult.Value(Ticker(s.toUpperCase)))(_.value)
    given Schema[Ticker]                  = Schema.string
  }

  enum Exchange(val code: String, val fullName: String):
    case NASDAQ extends Exchange("NSQ", "NASDAQ Stock Exchange")
    case NYSE   extends Exchange("NYS", "New York Stock Exchange")

  object Exchange extends EnumType[Exchange](() => Exchange.values, e => EnumType.printLowerCase(e))

  object SecurityKind extends EnumType[SecurityKind](() => SecurityKind.values, e => EnumType.printLowerCase(e))
  enum SecurityKind:
    /** Represents ownership in a single company (e.g., Apple, Microsoft). This is the most common type of security.
      */
    case Stock

    /** A basket of securities (like stocks or bonds) that tracks an index or sector. Trades on an exchange like a stock (e.g., SPY, QQQ).
      */
    case ETF

    /** A company that owns and typically operates income-producing real estate. Trades on an exchange like a stock.
      */
    case REIT

    /** A security that allows U.S. investors to trade shares of foreign companies on U.S. exchanges.
      */
    case ADR

    /** A catch-all for any other security type that is traded on an exchange but falls outside the common categories (e.g., Warrants,
      * Rights, Units). We acknowledge its existence but don't give it special status in our domain.
      */
    case Other

  final case class Security(
      ticker: Ticker,
      exchange: Exchange,
      name: String,
      kind: SecurityKind,
      isActive: Boolean = true,
      companyProfileLastUpdatedAt: Option[Instant] = None
  ) derives CirceCodec.AsObject

  final case class CompanyProfile(
      ticker: Ticker,
      name: String,
      country: String,
      industry: String,
      description: Option[String],
      website: String,
      ipoDate: Option[LocalDate],
      currency: String,
      marketCap: Long,
      priceAnalyticsLastUpdatedAt: Option[Instant] = None,
      financialMetricsLastUpdatedAt: Option[Instant] = None
  ) derives CirceCodec.AsObject

  final case class FinancialMetrics(
      ticker: Ticker,
      peTTM: Option[BigDecimal], // price-to-earnings ratio (trailing 12 months)
      epsTTM: Option[BigDecimal], // earnings per share (trailing 12 months)
      roeTTM: Option[BigDecimal], // return on equity; measures management efficiency
      dividendYieldIndicatedAnnual: Option[BigDecimal], // annual dividend payout percentage
      totalDebtToEquityAnnual: Option[BigDecimal], // total debt divided by shareholders' equity
      netProfitMarginTTM: Option[BigDecimal], // percentage of revenue remaining as profit
      freeCashFlowPerShareTTM: Option[BigDecimal], // cash generated per share after capital expenditures
      revenueGrowth5Y: Option[BigDecimal], // average annual revenue growth over 5 years
      epsGrowth5Y: Option[BigDecimal], // average annual earnings per share growth over 5 years
      fiftyTwoWeekHigh: Option[BigDecimal], // highest price reached in the last 52 weeks
      fiftyTwoWeekLow: Option[BigDecimal] // lowest price reached in the last 52 weeks
  ) derives CirceCodec.AsObject

  final case class Stock(
      security: Security,
      profile: Option[CompanyProfile],
      priceAnalytics: Option[PriceAnalytics] = None,
      financialMetrics: Option[FinancialMetrics] = None
  ) derives CirceCodec.AsObject

  final case class PriceCandle(
      date: LocalDate,
      open: BigDecimal,
      high: BigDecimal,
      low: BigDecimal,
      close: BigDecimal,
      volume: Long
  ) derives CirceCodec.AsObject

  final case class LatestPrice(
      ticker: Ticker,
      price: BigDecimal,
      date: LocalDate
  ) derives CirceCodec.AsObject
}