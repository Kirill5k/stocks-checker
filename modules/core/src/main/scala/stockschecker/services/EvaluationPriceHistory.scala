package stockschecker.services

import cats.effect.Sync
import cats.syntax.functor.*
import stockschecker.domain.{DateRange, EvaluationClose, EvaluationPriceSeries, PriceAdjustment, PriceHistorySnapshot, Ticker}

/** A daily, total-return history with source provenance and validation issues. */
trait EvaluationPriceHistory[F[_]]:
  def history(ticker: Ticker, range: DateRange): F[EvaluationPriceSeries]
  def cached(ticker: Ticker, range: DateRange): F[Option[EvaluationPriceSeries]]

final private class LiveEvaluationPriceHistory[F[_]: Sync](private val prices: PriceHistoryService[F]) extends EvaluationPriceHistory[F] {
  // Matches the output size requested from TwelveData; a full response may be truncated.
  private val providerCandleLimit = 5000

  private def toSeries(snapshot: PriceHistorySnapshot, range: DateRange): EvaluationPriceSeries =
    val issues = List(
      Option.when(snapshot.currency != "USD")("unsupported evaluation currency"),
      Option.when(snapshot.adjustment != PriceAdjustment.SplitsAndDividends)("total-return adjustments unavailable"),
      Option.when(snapshot.candles.size >= providerCandleLimit)("provider response may be truncated")
    ).flatten
    EvaluationPriceSeries(
      snapshot.ticker,
      snapshot.candles.filter(c => range.contains(c.date)).map(c => EvaluationClose(c.date, c.close)),
      snapshot.provider,
      snapshot.fetchedAt,
      issues.isEmpty,
      snapshot.adjustment.value,
      snapshot.requestedRange,
      issues,
      snapshotIds = List(snapshot.id)
    )

  override def cached(ticker: Ticker, range: DateRange): F[Option[EvaluationPriceSeries]] =
    prices.cachedHistorical(ticker, range).map(_.map(toSeries(_, range)))

  override def history(ticker: Ticker, range: DateRange): F[EvaluationPriceSeries] =
    prices.historical(ticker, range).map(toSeries(_, range))
}

object EvaluationPriceHistory:
  def make[F[_]: Sync](prices: PriceHistoryService[F]): F[EvaluationPriceHistory[F]] =
    Sync[F].pure(LiveEvaluationPriceHistory(prices))
