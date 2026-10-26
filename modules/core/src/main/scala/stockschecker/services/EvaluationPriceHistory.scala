package stockschecker.services

import cats.effect.Temporal
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{DateRange, EvaluationClose, EvaluationPriceSeries, PriceAdjustment, PriceHistorySnapshot, Ticker}
import stockschecker.repositories.PriceHistoryRepository
import java.time.{LocalDate, ZoneOffset}

/** A daily, total-return history with source provenance and validation issues. */
trait EvaluationPriceHistory[F[_]]:
  def history(ticker: Ticker, range: DateRange): F[EvaluationPriceSeries]
  def cached(ticker: Ticker, range: DateRange): F[Option[EvaluationPriceSeries]]

final private class LiveEvaluationPriceHistory[F[_]](
    private val client: MarketDataClient[F],
    private val repository: PriceHistoryRepository[F]
)(using
    F: Temporal[F]
) extends EvaluationPriceHistory[F] {

  private def coversCompletedSessions(snapshot: PriceHistorySnapshot, range: DateRange): Boolean =
    !snapshot.requestedRange.through.isBefore(range.through) &&
      !snapshot.fetchedAt.isBefore(range.through.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant)

  private def toSeries(snapshot: PriceHistorySnapshot, range: DateRange): EvaluationPriceSeries =
    val issues = List(
      Option.when(snapshot.currency != "USD")("unsupported evaluation currency"),
      Option.when(snapshot.adjustment != PriceAdjustment.SplitsAndDividends)("total-return adjustments unavailable"),
      Option.when(snapshot.candles.size >= 5000)("provider response may be truncated")
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
    repository
      .latestCovering(ticker, PriceAdjustment.SplitsAndDividends, range)
      .map(_.filter(coversCompletedSessions(_, range)).map(toSeries(_, range)))

  override def history(ticker: Ticker, range: DateRange): F[EvaluationPriceSeries] =
    for
      now <- Temporal[F].realTimeInstant
      today = LocalDate.ofInstant(now, ZoneOffset.UTC)
      // Fetch the shared analytics horizon once, so every screen creation date and
      // completed monthly cutoff can reuse the same unmerged adjustment vintage.
      sharedFrom    = if (range.from.isBefore(today.minusYears(13))) range.from else today.minusYears(13)
      sharedThrough = if (range.through.isAfter(today)) range.through else today
      sharedRange   = DateRange(sharedFrom, sharedThrough)
      stored <- repository.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, range)
      // Explicit jobs can refresh an incomplete vintage the next day. Other readers use cached(),
      // which preserves complete historical captures without an age limit.
      snapshot <- stored
        .filter(s => coversCompletedSessions(s, range) && !s.fetchedAt.isAfter(now) && s.fetchedAt.isAfter(now.minusSeconds(86400)))
        .map(_.pure[F])
        .getOrElse(client.getDailyPriceHistory(ticker, sharedRange, PriceAdjustment.SplitsAndDividends).flatTap(repository.save))
    yield toSeries(snapshot, range)
}

object EvaluationPriceHistory:
  def make[F[_]: Temporal](
      client: MarketDataClient[F],
      repository: PriceHistoryRepository[F]
  ): EvaluationPriceHistory[F] =
    LiveEvaluationPriceHistory(client, repository)
