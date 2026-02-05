package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PriceAnalytics, PriceAnalyticsFilter, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{LatestPriceRepository, PriceAnalyticsRepository}

import scala.concurrent.duration.*

trait PriceService[F[_]]:
  def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics]
  def fetchLatestPriceAnalytics(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LivePriceService[F[_]](
    private val repository: PriceAnalyticsRepository[F],
    private val latestPriceRepository: LatestPriceRepository[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends PriceService[F] {

  override def fetchLatestPriceAnalytics(tickers: NonEmptyList[Ticker]): F[Unit] =
    logger.info(s"Fetching price analytics for ${tickers.size} tickers") >>
      Stream
        .emits(tickers.toList)
        .metered(1.second)
        .evalMap { ticker =>
          fetchPriceAnalytics(ticker)
            .map(result => Some(result))
            .handleErrorWith {
              case AppError.HttpClient(_, 404, _) =>
                logger.warn(s"Ticker $ticker not found (404), deactivating security and company profile") >>
                  dispatcher.dispatch(Action.DeactivateSecurity(ticker)) >>
                  dispatcher.dispatch(Action.DeactivateCompanyProfile(ticker)).as(None)
              case error =>
                logger.error(s"Error fetching price analytics for $ticker: ${error.getMessage}").as(None)
            }
        }
        .unNone
        .chunkN(512)
        .evalMap { chunk =>
          val analyticsList = chunk.toList
          logger.info(s"Saving batch of ${analyticsList.size} price analytics") >>
            save(analyticsList)
        }
        .compile
        .drain >>
      logger.info(s"Finished fetching price analytics for ${tickers.size} tickers")

  override def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics] =
    if (fetch) fetchPriceAnalytics(ticker).flatTap(analytics => save(List(analytics)))
    else repository.find(ticker).flatMap(analytics => F.fromOption(analytics, AppError.PriceAnalyticsNotFound(ticker)))

  private def fetchPriceAnalytics(ticker: Ticker): F[PriceAnalytics] =
    for
      candles <- marketDataClient.getMonthlyPriceCandles(ticker)
      analytics = PriceAnalytics.from(ticker, candles)
    yield analytics

  private def save(analyticsList: List[PriceAnalytics]): F[Unit] =
    repository.save(analyticsList) >>
      latestPriceRepository.save(analyticsList.map(_.toLatestPrice)) >>
      dispatcher.dispatch(Action.RecordPriceAnalyticsUpdate(analyticsList.map(_.ticker)))

  override def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findBy(filter, limit)

  override def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findAll(limit)
}

object PriceService:
  def make[F[_]: {Temporal, Logger}](
      repository: PriceAnalyticsRepository[F],
      latestPriceRepository: LatestPriceRepository[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[PriceService[F]] =
    Temporal[F].pure(LivePriceService[F](repository, latestPriceRepository, marketDataClient, dispatcher))
