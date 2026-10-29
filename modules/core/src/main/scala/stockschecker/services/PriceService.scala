package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{DateRange, PriceAdjustment, PriceAnalytics, PriceAnalyticsFilter, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.domain.PriceAnalyticsAlignment
import stockschecker.repositories.{PriceAnalyticsRepository, PriceHistoryRepository}

import java.time.{Duration, Instant, LocalDate, Period, ZoneOffset}

trait PriceService[F[_]]:
  def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics]
  def refreshHistory(ticker: Ticker, through: LocalDate): F[Unit]

final private class LivePriceService[F[_]](
    private val repository: PriceAnalyticsRepository[F],
    private val dispatcher: ActionDispatcher[F],
    private val history: PriceHistoryRepository[F],
    private val client: MarketDataClient[F]
)(using F: Sync[F], C: Clock[F])
    extends PriceService[F] {

  override def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics] =
    if (fetch) fetchPriceAnalytics(ticker).flatTap(analytics => save(NonEmptyList.one(analytics)))
    else repository.find(ticker).flatMap(analytics => F.fromOption(analytics, AppError.PriceAnalyticsNotFound(ticker)))

  private def fetchPriceAnalytics(ticker: Ticker): F[PriceAnalytics] =
    for
      now <- C.now
      through    = LocalDate.ofInstant(now, ZoneOffset.UTC)
      totalRange = DateRange(through.minus(PriceService.SharedHistoryLookback), through)
      quoteRange = DateRange(through.minusDays(14), through)
      // Cache the small quote first; on provider deferral the retry can proceed directly to the large history.
      quoted    <- recent(ticker, quoteRange, PriceAdjustment.Unadjusted, now)
      total     <- recent(ticker, totalRange, PriceAdjustment.SplitsAndDividends, now)
      analytics <- F.fromEither(PriceAnalyticsAlignment.calculate(ticker, total, quoted, through))
    yield analytics

  override def refreshHistory(ticker: Ticker, through: LocalDate): F[Unit] =
    historical(ticker, DateRange(through.minus(PriceService.SharedHistoryLookback), through)).void

  private val maxSnapshotAge = Duration.ofDays(1)

  private def fresh(snapshot: PriceHistorySnapshot, asOf: Instant): Boolean =
    !snapshot.fetchedAt.isAfter(asOf) && snapshot.fetchedAt.isAfter(asOf.minus(maxSnapshotAge))

  private def fetch(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment): F[PriceHistorySnapshot] =
    client.getDailyPriceHistory(ticker, range, adjustment).flatTap(history.save)

  private def recent(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment, asOf: Instant): F[PriceHistorySnapshot] = {
    val cached = adjustment match
      case PriceAdjustment.Unadjusted => history.latest(ticker, adjustment)
      case _                          => history.latestCovering(ticker, adjustment, range)
    cached.flatMap { snapshot =>
      snapshot
        .filter { observation =>
          observation.ticker == ticker && observation.adjustment == adjustment && observation.currency.trim.nonEmpty &&
          fresh(observation, asOf) && !observation.requestedRange.from.isAfter(range.from) &&
          !observation.requestedRange.through.isBefore(range.through)
        }
        .map(_.pure[F])
        .getOrElse {
          // Cache the small quote so deferred analytics can resume; full adjusted histories belong to explicit captures.
          if adjustment == PriceAdjustment.Unadjusted then fetch(ticker, range, adjustment)
          else client.getDailyPriceHistory(ticker, range, adjustment)
        }
    }
  }

  private def historical(ticker: Ticker, range: DateRange): F[PriceHistorySnapshot] =
    for
      now <- C.now
      today       = LocalDate.ofInstant(now, ZoneOffset.UTC)
      sharedStart = today.minus(PriceService.SharedHistoryLookback)
      sharedRange = DateRange(
        if range.from.isBefore(sharedStart) then range.from else sharedStart,
        if range.through.isAfter(today) then range.through else today
      )
      stored   <- history.latestCovering(ticker, PriceAdjustment.SplitsAndDividends, range)
      snapshot <- stored
        .filter(s => s.coversCompletedSessions(range) && fresh(s, now))
        .fold(fetch(ticker, sharedRange, PriceAdjustment.SplitsAndDividends))(_.pure[F])
    yield snapshot

  private def save(analyticsList: NonEmptyList[PriceAnalytics]): F[Unit] =
    repository.save(analyticsList.toList) >>
      dispatcher.dispatch(Action.RecordPriceAnalyticsUpdate(analyticsList.map(_.ticker)))

  override def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findBy(filter, limit)

  override def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findAll(limit)
}

object PriceService:
  val SharedHistoryLookback: Period = Period.ofYears(13)

  def make[F[_]: {Sync, Clock}](
      repository: PriceAnalyticsRepository[F],
      dispatcher: ActionDispatcher[F],
      history: PriceHistoryRepository[F],
      client: MarketDataClient[F]
  ): F[PriceService[F]] =
    Sync[F].pure(LivePriceService[F](repository, dispatcher, history, client))
