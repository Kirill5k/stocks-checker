package stockschecker.services

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{FinancialMetrics, Ticker}
import stockschecker.repositories.{CompanyProfileRepository, FinancialHistoryRepository, FinancialMetricsRepository}

trait FinancialMetricsService[F[_]]:
  def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics]

final private class LiveFinancialMetricsService[F[_]](
    private val repository: FinancialMetricsRepository[F],
    private val client: MarketDataClient[F],
    private val profiles: CompanyProfileRepository[F],
    private val history: FinancialHistoryRepository[F]
)(using
    F: Sync[F]
) extends FinancialMetricsService[F] {

  override def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics] =
    val result =
      if fetch then
        client.getFinancialData(ticker).flatMap {
          case Some(data) =>
            history.save(data.snapshot) >> repository.save(List(data.metrics)) >>
              profiles.updateFinancialMetricsLastUpdated(List(ticker)).as(Some(data.metrics))
          case None => F.pure(None)
        }
      else repository.find(ticker)
    result.flatMap(value => F.fromOption(value, AppError.FinancialMetricsNotFound(ticker)))
}

object FinancialMetricsService:
  def make[F[_]: Sync](
      repository: FinancialMetricsRepository[F],
      client: MarketDataClient[F],
      profiles: CompanyProfileRepository[F],
      history: FinancialHistoryRepository[F]
  ): F[FinancialMetricsService[F]] =
    Sync[F].pure(LiveFinancialMetricsService[F](repository, client, profiles, history))
