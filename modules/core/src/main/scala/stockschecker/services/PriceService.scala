package stockschecker.services

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PricePerformanceSummary, Ticker}
import stockschecker.repositories.PricePerformanceSummaryRepository

trait PriceService[F[_]]:
  def fetchLatestPerformanceSummary(ticker: Ticker): F[Unit]

final private class LivePriceService[F[_]: Concurrent](
    private val repository: PricePerformanceSummaryRepository[F],
    private val marketDataClient: MarketDataClient[F]
) extends PriceService[F] {

  override def fetchLatestPerformanceSummary(ticker: Ticker): F[Unit] =
    marketDataClient
        .getMonthlyPriceCandles(ticker)
        .flatMap(candles => repository.save(PricePerformanceSummary.from(ticker, candles)))
}

object PriceService:
  def make[F[_]](
      repository: PricePerformanceSummaryRepository[F],
      marketDataClient: MarketDataClient[F]
  )(using Concurrent[F]): F[PriceService[F]] =
    Monad[F].pure(LivePriceService[F](repository, marketDataClient))

