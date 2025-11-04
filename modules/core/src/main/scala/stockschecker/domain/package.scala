package stockschecker

import cats.data.NonEmptyList
import cats.implicits.toFoldableOps
import io.circe.Codec as CirceCodec
import stockschecker.common.types.{EnumType, StringType}
import stockschecker.domain.errors.AppError
import sttp.tapir.{Codec, DecodeResult, Schema}

import java.time.{Instant, LocalDate}
import scala.math.BigDecimal.RoundingMode

package object domain {

  opaque type Ticker = String
  object Ticker extends StringType[Ticker] {
    inline given Codec.PlainCodec[Ticker] = Codec.string.mapDecode[Ticker](s => DecodeResult.Value(Ticker(s.toUpperCase)))(_.value)
    given Schema[Ticker]                  = Schema.string
  }

  enum Exchange(val code: String, val fullName: String):
    case NASDAQ extends Exchange("NSQ", "NASDAQ Stock Exchange")
    case NYSE   extends Exchange("NYS", "New York Stock Exchange")

  object Exchange extends EnumType[Exchange](() => Exchange.values, e => EnumType.printLowerCase(e)) {
    inline given Codec.PlainCodec[Exchange] = Codec.string.mapDecode[Exchange](s =>
      from(s) match {
        case Right(exchange) => DecodeResult.Value(exchange)
        case Left(error)     => DecodeResult.Error(s, AppError.FailedValidation(error))
      }
    )(_.print)
  }

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
      ipoDate: LocalDate,
      currency: String,
      marketCap: Long,
      pricePerformanceLastUpdatedAt: Option[Instant] = None
  ) derives CirceCodec.AsObject

  final case class Stock(
      security: Security,
      profile: CompanyProfile
  ) derives CirceCodec.AsObject

  final case class PriceCandle(
      date: LocalDate,
      open: BigDecimal,
      high: BigDecimal,
      low: BigDecimal,
      close: BigDecimal,
      volume: Long
  ) derives CirceCodec.AsObject

  final case class PricePerformanceSummary(
      ticker: Ticker,
      latestPrice: BigDecimal,
      latestPriceDate: LocalDate,
      oneMonthChange: Option[BigDecimal],
      threeMonthChange: Option[BigDecimal],
      sixMonthChange: Option[BigDecimal],
      oneYearChange: Option[BigDecimal],
      threeYearChange: Option[BigDecimal],
      fiveYearChange: Option[BigDecimal],
      tenYearChange: Option[BigDecimal],
      maxChange: Option[BigDecimal]
  ) derives CirceCodec.AsObject

  object PricePerformanceSummary:
    def from(ticker: Ticker, priceCandles: NonEmptyList[PriceCandle]): PricePerformanceSummary = {
      val latestCandle    = priceCandles.head
      val latestPrice     = latestCandle.close
      val latestPriceDate = latestCandle.date

      def calculateChange(prevCandle: Option[PriceCandle]): Option[BigDecimal] =
        prevCandle.flatMap { lastCandle =>
          val earliestPrice = lastCandle.close
          Option.when(earliestPrice > 0) {
            val change = (latestPrice - earliestPrice) / earliestPrice
            change.setScale(4, RoundingMode.HALF_UP)
          }
        }

      def calculatePeriodChange(monthsAgo: Int): Option[BigDecimal] =
        // NonEmptyList.get(n) safely returns an Option[PriceCandle].
        // This elegantly handles cases where history is shorter than the look-back period.
        calculateChange(priceCandles.get(monthsAgo))

      PricePerformanceSummary(
        ticker = ticker,
        latestPrice = latestPrice,
        latestPriceDate = latestPriceDate,
        oneMonthChange = calculatePeriodChange(1),
        threeMonthChange = calculatePeriodChange(3),
        sixMonthChange = calculatePeriodChange(6),
        oneYearChange = calculatePeriodChange(12),
        threeYearChange = calculatePeriodChange(36),
        fiveYearChange = calculatePeriodChange(60),
        tenYearChange = calculatePeriodChange(120),
        maxChange = calculateChange(priceCandles.tail.lastOption)
      )
    }
}
