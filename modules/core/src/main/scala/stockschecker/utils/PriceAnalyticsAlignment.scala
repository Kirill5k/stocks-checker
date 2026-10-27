package stockschecker.utils

import cats.data.NonEmptyList
import stockschecker.domain.{PriceAdjustment, PriceAnalytics, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError

import java.time.LocalDate
import java.time.temporal.ChronoUnit

object PriceAnalyticsAlignment:
  def calculate(
      ticker: Ticker,
      total: PriceHistorySnapshot,
      quoted: PriceHistorySnapshot,
      through: LocalDate
  ): Either[AppError, PriceAnalytics] =
    def validIdentity(snapshot: PriceHistorySnapshot, adjustment: PriceAdjustment): Boolean =
      snapshot.ticker == ticker && snapshot.adjustment == adjustment && snapshot.currency.trim.nonEmpty

    for
      _ <- Either.cond(
        validIdentity(total, PriceAdjustment.SplitsAndDividends) && validIdentity(quoted, PriceAdjustment.Unadjusted) &&
          total.currency.equalsIgnoreCase(quoted.currency),
        (),
        AppError.FailedValidation("Adjusted and quoted price histories must have matching ticker, currency and adjustment metadata")
      )
      bars        <- NonEmptyList.fromList(total.candles).toRight(AppError.PriceAnalyticsNotFound(ticker))
      valid       <- PriceAnalyticsCalculator.validate(bars, monthly = false).left.map(AppError.FailedValidation(_))
      quoteBars   <- NonEmptyList.fromList(quoted.candles).toRight(AppError.PriceAnalyticsNotFound(ticker))
      validQuotes <- PriceAnalyticsCalculator.validate(quoteBars, monthly = false).left.map(AppError.FailedValidation(_))
      // A deferred quote request may finish in a later session; both series must use the same endpoint.
      quotesByDate = validQuotes.toList.map(c => c.date -> c).toMap
      common <- valid.toList.iterator
        .filterNot(_.date.isAfter(through))
        .flatMap(candle => quotesByDate.get(candle.date).map(quote => candle -> quote))
        .nextOption()
        .toRight(AppError.DataUnavailable("TwelveData", ticker, "adjusted and quoted histories have no common trading session"))
      (endpoint, quote) = common
      _ <- Either.cond(
        ChronoUnit.DAYS.between(endpoint.date, through) <= 7,
        (),
        AppError.DataUnavailable("TwelveData", ticker, "the latest matching trading session is more than seven days old")
      )
      aligned   = NonEmptyList(endpoint, valid.toList.filter(_.date.isBefore(endpoint.date)))
      analytics = PriceAnalyticsCalculator.fromDaily(ticker, aligned)
    yield analytics.copy(
      performanceSummary = analytics.performanceSummary.copy(latestPrice = quote.close, latestPriceDate = quote.date),
      basis = analytics.basis.map(_.copy(priceObservationId = Some(total.id), quotedPriceObservationId = Some(quoted.id)))
    )
