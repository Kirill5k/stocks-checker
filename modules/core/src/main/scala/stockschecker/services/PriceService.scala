package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{DateRange, PriceAdjustment, PriceAnalytics, PriceAnalyticsFilter, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.domain.PriceAnalyticsAlignment
import stockschecker.repositories.{LatestPriceRepository, PriceAnalyticsRepository}

import java.time.{LocalDate, ZoneOffset}

trait PriceService[F[_]]:
  def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics]

final private class LivePriceService[F[_]](
    private val repository: PriceAnalyticsRepository[F],
    private val latestPriceRepository: LatestPriceRepository[F],
    private val dispatcher: ActionDispatcher[F],
    private val history: PriceHistoryService[F]
)(using F: Sync[F], C: Clock[F])
    extends PriceService[F] {

  override def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics] =
    if (fetch) fetchPriceAnalytics(ticker).flatTap(analytics => save(NonEmptyList.one(analytics)))
    else repository.find(ticker).flatMap(analytics => F.fromOption(analytics, AppError.PriceAnalyticsNotFound(ticker)))

  private def fetchPriceAnalytics(ticker: Ticker): F[PriceAnalytics] =
    for
      now <- C.now
      through    = LocalDate.ofInstant(now, ZoneOffset.UTC)
      totalRange = DateRange(through.minus(PriceHistoryService.SharedHistoryLookback), through)
      quoteRange = DateRange(through.minusDays(14), through)
      total     <- history.recent(ticker, totalRange, PriceAdjustment.SplitsAndDividends, now)
      quoted    <- history.recent(ticker, quoteRange, PriceAdjustment.Unadjusted, now)
      analytics <- F.fromEither(PriceAnalyticsAlignment.calculate(ticker, total, quoted, through))
    yield analytics

  private def save(analyticsList: NonEmptyList[PriceAnalytics]): F[Unit] =
    repository.save(analyticsList.toList) >>
      latestPriceRepository.save(analyticsList.toList.map(_.toLatestPrice)) >>
      dispatcher.dispatch(Action.RecordPriceAnalyticsUpdate(analyticsList.map(_.ticker)))

  override def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findBy(filter, limit)

  override def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findAll(limit)
}

object PriceService:
  def make[F[_]: {Sync, Clock}](
      repository: PriceAnalyticsRepository[F],
      latestPriceRepository: LatestPriceRepository[F],
      dispatcher: ActionDispatcher[F],
      history: PriceHistoryService[F]
  ): F[PriceService[F]] =
    Sync[F].pure(LivePriceService[F](repository, latestPriceRepository, dispatcher, history))
