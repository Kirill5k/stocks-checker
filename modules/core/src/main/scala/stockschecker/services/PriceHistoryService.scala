package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{DateRange, PriceAdjustment, PriceHistorySnapshot, Ticker}
import stockschecker.repositories.PriceHistoryRepository

import java.time.{Duration, Instant, LocalDate, Period, ZoneOffset}

trait PriceHistoryService[F[_]]:
  def recent(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment, asOf: Instant): F[PriceHistorySnapshot]
  def historical(ticker: Ticker, range: DateRange): F[PriceHistorySnapshot]
  def cachedHistorical(ticker: Ticker, range: DateRange): F[Option[PriceHistorySnapshot]]

final private class LivePriceHistoryService[F[_]](
    private val client: MarketDataClient[F],
    private val repository: PriceHistoryRepository[F]
)(using F: Sync[F], C: Clock[F])
    extends PriceHistoryService[F] {
  private val maxSnapshotAge = Duration.ofDays(1)

  private def fresh(snapshot: PriceHistorySnapshot, asOf: Instant): Boolean =
    !snapshot.fetchedAt.isAfter(asOf) && snapshot.fetchedAt.isAfter(asOf.minus(maxSnapshotAge))

  private def coversCompletedSessions(snapshot: PriceHistorySnapshot, range: DateRange): Boolean =
    !snapshot.requestedRange.through.isBefore(range.through) &&
      !snapshot.fetchedAt.isBefore(range.through.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant)

  private def fetch(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment): F[PriceHistorySnapshot] =
    client.getDailyPriceHistory(ticker, range, adjustment).flatTap(repository.save)

  override def recent(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment, asOf: Instant): F[PriceHistorySnapshot] = {
    val cached = adjustment match
      case PriceAdjustment.Unadjusted => repository.latest(ticker, adjustment)
      case _                          => repository.latestCovering(ticker, adjustment, range)
    cached.flatMap(
      _.filter(snapshot =>
        snapshot.ticker == ticker && snapshot.adjustment == adjustment && snapshot.currency.trim.nonEmpty &&
          fresh(snapshot, asOf) && !snapshot.requestedRange.from.isAfter(range.from) &&
          !snapshot.requestedRange.through.isBefore(range.through)
      ).fold(fetch(ticker, range, adjustment))(_.pure[F])
    )
  }

  override def cachedHistorical(ticker: Ticker, range: DateRange): F[Option[PriceHistorySnapshot]] =
    repository
      .latestCovering(ticker, PriceAdjustment.SplitsAndDividends, range)
      .map(_.filter(coversCompletedSessions(_, range)))

  override def historical(ticker: Ticker, range: DateRange): F[PriceHistorySnapshot] =
    for
      now <- C.now
      today       = LocalDate.ofInstant(now, ZoneOffset.UTC)
      sharedStart = today.minus(PriceHistoryService.SharedHistoryLookback)
      sharedRange = DateRange(
        if (range.from.isBefore(sharedStart)) range.from else sharedStart,
        if (range.through.isAfter(today)) range.through else today
      )
      stored <- cachedHistorical(ticker, range)
      // Explicit capture refreshes old observations; cache-only historical reads have no age limit.
      snapshot <- stored.filter(fresh(_, now)).fold(fetch(ticker, sharedRange, PriceAdjustment.SplitsAndDividends))(_.pure[F])
    yield snapshot
}

object PriceHistoryService:
  // One unmerged adjustment vintage serves live analytics and historical evaluation captures.
  val SharedHistoryLookback: Period = Period.ofYears(13)

  def make[F[_]: {Sync, Clock}](client: MarketDataClient[F], repository: PriceHistoryRepository[F]): F[PriceHistoryService[F]] =
    Sync[F].pure(LivePriceHistoryService(client, repository))
