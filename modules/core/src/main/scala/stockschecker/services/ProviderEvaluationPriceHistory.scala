package stockschecker.services

import cats.effect.Temporal
import cats.syntax.all.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{DateRange, EvaluationClose, EvaluationPriceSeries, PriceAdjustment, PriceHistorySnapshot, Ticker}
import stockschecker.repositories.PriceHistoryRepository
import java.time.{LocalDate, ZoneOffset}

object ProviderEvaluationPriceHistory {
  def make[F[_]: Temporal](
      client: MarketDataClient[F],
      repository: PriceHistoryRepository[F],
      audited: Boolean
  ): EvaluationPriceHistory[F] =
    new EvaluationPriceHistory[F] {
      private def coversCompletedSessions(snapshot: PriceHistorySnapshot, through: LocalDate): Boolean =
        !snapshot.requestedThrough.isBefore(through) &&
          !snapshot.fetchedAt.isBefore(through.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant)
      private def toSeries(snapshot: PriceHistorySnapshot, from: LocalDate, through: LocalDate): EvaluationPriceSeries = {
        val issues = List(
          Option.when(!audited)("total-return provider contract has not been audited"),
          Option.when(snapshot.currency != "USD")("unsupported evaluation currency"),
          Option.when(snapshot.candles.size >= 5000)("provider response may be truncated")
        ).flatten
        EvaluationPriceSeries(
          snapshot.ticker,
          snapshot.candles.filter(c => !c.date.isBefore(from) && !c.date.isAfter(through)).map(c => EvaluationClose(c.date, c.close)),
          snapshot.provider,
          snapshot.fetchedAt,
          audited && issues.isEmpty,
          snapshot.adjustment.value,
          snapshot.requestedFrom,
          snapshot.requestedThrough,
          issues,
          snapshotIds = List(snapshot.id)
        )
      }

      def cached(ticker: Ticker, from: LocalDate, through: LocalDate): F[Option[EvaluationPriceSeries]] =
        repository
          .latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from)
          .map(_.filter(coversCompletedSessions(_, through)).map(toSeries(_, from, through)))

      def history(ticker: Ticker, from: LocalDate, through: LocalDate): F[EvaluationPriceSeries] =
        for
          now <- Temporal[F].realTimeInstant
          today = LocalDate.ofInstant(now, ZoneOffset.UTC)
          // Fetch the shared analytics horizon once, so every screen creation date and
          // completed monthly cutoff can reuse the same unmerged adjustment vintage.
          sharedFrom    = if (from.isBefore(today.minusYears(13))) from else today.minusYears(13)
          sharedThrough = if (through.isAfter(today)) through else today
          range         = DateRange(sharedFrom, sharedThrough)
          stored <- repository.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, from)
          // Explicit jobs can refresh an incomplete vintage the next day. Other readers use cached(),
          // which preserves complete historical captures without an age limit.
          snapshot <- stored
            .filter(s => coversCompletedSessions(s, through) && !s.fetchedAt.isAfter(now) && s.fetchedAt.isAfter(now.minusSeconds(86400)))
            .fold(
              client.getDailyPriceHistory(ticker, range, PriceAdjustment.SplitsAndDividends).flatTap(repository.save)
            )(_.pure[F])
        yield toSeries(snapshot, from, through)
    }
}
