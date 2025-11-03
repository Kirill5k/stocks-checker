package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{PricePerformanceSummary, Ticker}
import stockschecker.repositories.PricePerformanceSummaryRepository

import scala.concurrent.duration.*

trait PriceService[F[_]]:
  def findPerformanceSummary(ticker: Ticker, fetch: Boolean): F[PricePerformanceSummary]
  def fetchLatestPerformanceSummaries(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LivePriceService[F[_]](
    private val repository: PricePerformanceSummaryRepository[F],
    private val marketDataClient: MarketDataClient[F]
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
              logger.error(error)(s"Error fetching price performance summary for $ticker").as(None)
            }
        }
        .unNone
        .chunkN(512)
        .evalMap { chunk =>
          logger.info(s"Saving batch of ${chunk.size} price performance summaries") >>
            repository.save(chunk.toList)
        }
        .compile
        .drain >>
      logger.info(s"Finished fetching price performance summaries for ${tickers.size} tickers")

  override def findPerformanceSummary(ticker: Ticker, fetch: Boolean): F[PricePerformanceSummary] =
    if (fetch) fetchPerformanceSummary(ticker).flatTap(repository.save)
    else repository.find(ticker).flatMap(pps => F.fromOption(pps, AppError.PricePerformanceSummaryNotFound(ticker)))

  private def fetchPerformanceSummary(ticker: Ticker): F[PricePerformanceSummary] =
    marketDataClient
      .getMonthlyPriceCandles(ticker)
      .map(candles => PricePerformanceSummary.from(ticker, candles))
}

object PriceService:
  def make[F[_]: {Temporal, Logger}](
      repository: PricePerformanceSummaryRepository[F],
      marketDataClient: MarketDataClient[F]
  ): F[PriceService[F]] =
    Temporal[F].pure(LivePriceService[F](repository, marketDataClient))
