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
import stockschecker.repositories.{FinancialHistoryRepository, FinancialMetricsRepository}

import scala.concurrent.duration.*

trait FinancialMetricsService[F[_]]:
  def fetchLatest(tickers: NonEmptyList[Ticker]): F[Unit]
  def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics]

final private class LiveFinancialMetricsService[F[_]](
    private val repository: FinancialMetricsRepository[F],
    private val client: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F],
    private val history: Option[FinancialHistoryRepository[F]]
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
          fetchAndSave(ticker).attempt
        }
        .compile
        .toList
        .flatMap { outcomes =>
          outcomes.collectFirst { case Left(error) => error }.fold(F.unit)(F.raiseError[Unit])
        } >>
      logger.info(s"Finished fetching financial metrics for ${tickers.size} tickers")

  private def fetchAndSave(ticker: Ticker): F[Option[FinancialMetrics]] =
    history match
      case Some(repository) =>
        client.getFinancialData(ticker).flatMap {
          case Some(data) => repository.save(data.snapshot) >> save(List(data.metrics)).as(Some(data.metrics))
          case None       => F.pure(None)
        }
      case None =>
        client.getFinancialMetrics(ticker).flatMap {
          case Some(metrics) => save(List(metrics)).as(Some(metrics))
          case None          => F.pure(None)
        }

  private def save(metrics: List[FinancialMetrics]): F[Unit] =
    repository.save(metrics) >>
      dispatcher.dispatch(Action.RecordFinancialMetricsUpdate(metrics.map(_.ticker)))

  override def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics] = {
    val mapFromOpt = (metrics: Option[FinancialMetrics]) => F.fromOption(metrics, AppError.FinancialMetricsNotFound(ticker))
    if (fetch) fetchAndSave(ticker).flatMap(mapFromOpt)
    else repository.find(ticker).flatMap(mapFromOpt)
  }
}

object FinancialMetricsService:
  def make[F[_]: {Temporal, Logger}](
      repo: FinancialMetricsRepository[F],
      client: MarketDataClient[F],
      dispatcher: ActionDispatcher[F],
      history: Option[FinancialHistoryRepository[F]] = None
  ): F[FinancialMetricsService[F]] =
    Temporal[F].pure(LiveFinancialMetricsService[F](repo, client, dispatcher, history))
