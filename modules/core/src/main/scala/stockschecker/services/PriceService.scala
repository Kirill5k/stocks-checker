package stockschecker.services

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.{DateRange, PriceAdjustment, PriceAnalytics, PriceAnalyticsFilter, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.domain.PriceAnalyticsAlignment
import stockschecker.repositories.{CompanyProfileRepository, LatestPriceRepository, PriceAnalyticsRepository}

import java.time.{LocalDate, ZoneOffset}

trait PriceService[F[_]]:
  def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics]

final private class LivePriceService[F[_]](
    private val repository: PriceAnalyticsRepository[F],
    private val latestPriceRepository: LatestPriceRepository[F],
    private val profiles: CompanyProfileRepository[F],
    private val history: PriceHistoryService[F]
)(using F: Sync[F], C: Clock[F])
    extends PriceService[F] {

  override def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics] =
    if (fetch) fetchPriceAnalytics(ticker).flatTap(analytics => save(List(analytics)))
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

  private def save(analyticsList: List[PriceAnalytics]): F[Unit] =
    repository.save(analyticsList) >>
      latestPriceRepository.save(analyticsList.map(_.toLatestPrice)) >>
      profiles.updatePriceAnalyticsLastUpdated(analyticsList.map(_.ticker))

  override def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findBy(filter, limit)

  override def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findAll(limit)
}

object PriceService:
  def make[F[_]: {Sync, Clock}](
      repository: PriceAnalyticsRepository[F],
      latestPriceRepository: LatestPriceRepository[F],
      profiles: CompanyProfileRepository[F],
      history: PriceHistoryService[F]
  ): F[PriceService[F]] =
    Sync[F].pure(LivePriceService[F](repository, latestPriceRepository, profiles, history))
