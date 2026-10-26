package stockschecker.services

import cats.effect.Temporal
import cats.syntax.all.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{EvaluationClose, EvaluationPriceSeries, PriceHistorySnapshot, Ticker}
import stockschecker.repositories.PriceHistoryRepository
import java.time.{LocalDate, ZoneOffset}

object ProviderEvaluationPriceHistory {
  def make[F[_]: Temporal](client: MarketDataClient[F], repository: PriceHistoryRepository[F], audited: Boolean): EvaluationPriceHistory[F] =
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
        EvaluationPriceSeries(snapshot.ticker,
          snapshot.candles.filter(c => !c.date.isBefore(from) && !c.date.isAfter(through)).map(c => EvaluationClose(c.date, c.close)),
          snapshot.provider, snapshot.fetchedAt, audited && issues.isEmpty, snapshot.adjustment,
          snapshot.requestedFrom, snapshot.requestedThrough, issues, snapshotIds = List(snapshot.id))
      }

      def cached(ticker: Ticker, from: LocalDate, through: LocalDate): F[Option[EvaluationPriceSeries]] =
        repository.latestCovering(ticker, "split-and-dividend", from)
          .map(_.filter(coversCompletedSessions(_, through)).map(toSeries(_, from, through)))

      def history(ticker: Ticker, from: LocalDate, through: LocalDate): F[EvaluationPriceSeries] =
        for
          stored <- repository.latestCovering(ticker, "split-and-dividend", from)
          // A fixed evaluation cutoff needs only one complete vintage, regardless of today's date.
          snapshot <- stored.filter(coversCompletedSessions(_, through)).fold(
            client.getDailyPriceHistory(ticker, from, through, "all").flatTap(repository.save)
          )(_.pure[F])
        yield toSeries(snapshot, from, through)
    }
}
