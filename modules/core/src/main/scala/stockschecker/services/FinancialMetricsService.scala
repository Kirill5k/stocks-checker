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
import stockschecker.domain.{FinancialMetrics, Ticker}
import stockschecker.repositories.FinancialMetricsRepository

import scala.concurrent.duration.*

trait FinancialMetricsService[F[_]]:
  def fetchLatest(tickers: NonEmptyList[Ticker]): F[Unit]
  def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics]

final private class LiveFinancialMetricsService[F[_]](
    private val repository: FinancialMetricsRepository[F],
    private val client: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends FinancialMetricsService[F] {

  override def fetchLatest(tickers: NonEmptyList[Ticker]): F[Unit] =
    logger.info(s"Fetching latest financial metrics for ${tickers.size} tickers") >>
      Stream
        .emits(tickers.toList)
        .metered(1.second)
        .evalMap { ticker =>
          client
            .getFinancialMetrics(ticker)
            .handleErrorWith { error =>
              logger.error(error)(s"Error fetching financial metrics for $ticker").as(None)
            }
        }
        .unNone
        .chunkN(512)
        .evalMap { chunk =>
          val metricsList = chunk.toList
          logger.info(s"Saving batch of ${metricsList.size} financial metrics") >>
            save(metricsList)
        }
        .compile
        .drain >>
      logger.info(s"Finished fetching financial metrics for ${tickers.size} tickers")

  private def save(metrics: List[FinancialMetrics]): F[Unit] =
    repository.save(metrics) >>
      dispatcher.dispatch(Action.RecordFinancialMetricsUpdate(metrics.map(_.ticker)))

  override def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics] = {
    val mapFromOpt = (metrics: Option[FinancialMetrics]) => F.fromOption(metrics, AppError.FinancialMetricsNotFound(ticker))
    if (fetch) client.getFinancialMetrics(ticker).flatMap(mapFromOpt).flatTap(metrics => save(List(metrics)))
    else repository.find(ticker).flatMap(mapFromOpt)
  }
}

object FinancialMetricsService:
  def make[F[_]: {Temporal, Logger}](
      repo: FinancialMetricsRepository[F],
      client: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[FinancialMetricsService[F]] =
    Temporal[F].pure(LiveFinancialMetricsService[F](repo, client, dispatcher))
