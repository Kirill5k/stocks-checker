package stockschecker.services

import cats.effect.Sync
import cats.syntax.all.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{FinancialMetrics, Ticker}
import stockschecker.repositories.{CompanyProfileRepository, FinancialHistoryRepository, FinancialMetricsRepository}

trait FinancialMetricsService[F[_]]:
  def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics]

object FinancialMetricsService:
  def make[F[_]: Sync](
      repository: FinancialMetricsRepository[F],
      client: MarketDataClient[F],
      profiles: CompanyProfileRepository[F],
      history: FinancialHistoryRepository[F]
  ): F[FinancialMetricsService[F]] =
    Sync[F].pure(new FinancialMetricsService[F] {
      def find(ticker: Ticker, fetch: Boolean): F[FinancialMetrics] =
        val result =
          if fetch then
            client.getFinancialData(ticker).flatMap {
              case Some(data) =>
                history.save(data.snapshot) >> repository.save(List(data.metrics)) >>
                  profiles.updateFinancialMetricsLastUpdated(List(ticker)).as(Some(data.metrics))
              case None => Sync[F].pure(None)
            }
          else repository.find(ticker)
        result.flatMap(value => Sync[F].fromOption(value, AppError.FinancialMetricsNotFound(ticker)))
    })
