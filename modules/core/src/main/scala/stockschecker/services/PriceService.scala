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
import stockschecker.domain.errors.AppError
import stockschecker.domain.{PricePerformanceFilter, PricePerformanceSummary, Ticker}
import stockschecker.repositories.PricePerformanceSummaryRepository

import scala.concurrent.duration.*

trait PriceService[F[_]]:
  def findPerformanceSummariesBy(filter: PricePerformanceFilter, limit: Option[Int]): F[List[PricePerformanceSummary]]
  def findPerformanceSummary(ticker: Ticker, fetch: Boolean): F[PricePerformanceSummary]
  def fetchLatestPerformanceSummaries(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LivePriceService[F[_]](
    private val repository: PricePerformanceSummaryRepository[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends PriceService[F] {

  override def fetchLatestPerformanceSummaries(tickers: NonEmptyList[Ticker]): F[Unit] =
    logger.info(s"Fetching price performance summaries for ${tickers.size} tickers") >>
      Stream
        .emits(tickers.toList)
        .metered(1.second)
        .evalMap { ticker =>
          fetchPerformanceSummary(ticker)
            .map(pps => Some(pps))
            .handleErrorWith { error =>
              logger.error(s"Error fetching price performance summary for $ticker: ${error.getMessage}").as(None)
            }
        }
        .unNone
        .chunkN(512)
        .evalMap { chunk =>
          logger.info(s"Saving batch of ${chunk.size} price performance summaries") >> save(chunk.toList)
        }
        .compile
        .drain >>
      logger.info(s"Finished fetching price performance summaries for ${tickers.size} tickers")

  override def findPerformanceSummary(ticker: Ticker, fetch: Boolean): F[PricePerformanceSummary] =
    if (fetch) fetchPerformanceSummary(ticker).flatTap(pps => save(List(pps)))
    else repository.find(ticker).flatMap(pps => F.fromOption(pps, AppError.PricePerformanceSummaryNotFound(ticker)))

  private def fetchPerformanceSummary(ticker: Ticker): F[PricePerformanceSummary] =
    marketDataClient
      .getMonthlyPriceCandles(ticker)
      .map(candles => PricePerformanceSummary.from(ticker, candles))

  private def save(ppss: List[PricePerformanceSummary]): F[Unit] =
    repository.save(ppss) >> dispatcher.dispatch(Action.RecordPricePerformanceUpdate(ppss.map(_.ticker)))

  override def findPerformanceSummariesBy(filter: PricePerformanceFilter, limit: Option[Int]): F[List[PricePerformanceSummary]] =
    F.pure(Nil)
}

object PriceService:
  def make[F[_]: {Temporal, Logger}](
      repository: PricePerformanceSummaryRepository[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[PriceService[F]] =
    Temporal[F].pure(LivePriceService[F](repository, marketDataClient, dispatcher))
