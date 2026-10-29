package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.MarketDataClient
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.errors.AppError
import stockschecker.domain.{FinancialMetrics, Ticker}
import stockschecker.repositories.{FinancialHistoryRepository, FinancialMetricsRepository}

trait FinancialMetricsService[F[_]]:
  def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics]

final private class LiveFinancialMetricsService[F[_]](
    private val metricsRepository: FinancialMetricsRepository[F],
    private val client: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F],
    private val historyRepository: FinancialHistoryRepository[F]
)(using
    F: Sync[F]
) extends FinancialMetricsService[F] {

  override def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics] =
    val result =
      if fetch then
        client.getFinancialData(ticker).flatMap {
          case Some(data) =>
            historyRepository.save(data.snapshot) >>
              metricsRepository.save(List(data.metrics)) >>
              dispatcher.dispatch(Action.RecordFinancialMetricsUpdate(NonEmptyList.one(ticker))).as(Some(data.metrics))
          case None => F.pure(None)
        }
      else metricsRepository.find(ticker)
    result.flatMap(value => F.fromOption(value, AppError.FinancialMetricsNotFound(ticker)))
}

object FinancialMetricsService:
  def make[F[_]: Sync](
      repository: FinancialMetricsRepository[F],
      client: MarketDataClient[F],
      dispatcher: ActionDispatcher[F],
      history: FinancialHistoryRepository[F]
  ): F[FinancialMetricsService[F]] =
    Sync[F].pure(LiveFinancialMetricsService[F](repository, client, dispatcher, history))
