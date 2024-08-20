package stockschecker.services

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Stock, Ticker}
import stockschecker.repositories.StockRepository

trait StockService[F[_]]:
  def fetchLatest: F[Unit]
  def get(ticker: Ticker): F[Stock]

final private class LiveStockService[F[_]](
    private val repository: StockRepository[F],
    private val marketDataClient: MarketDataClient[F]
)(using
    F: Concurrent[F]
) extends StockService[F] {
  override def fetchLatest: F[Unit] =
    marketDataClient
      .getAllTradedStocks
      .chunkN(1024)
      .mapAsync(4)(chunk => repository.save(chunk.toList))
      .compile
      .drain

  override def get(ticker: Ticker): F[Stock] =
    repository
      .find(ticker)
      .flatMap(s => F.fromOption(s, AppError.StockNotFound(ticker)))
}

object StockService:
  def make[F[_]](repo: StockRepository[F], client: MarketDataClient[F])(using F: Concurrent[F]): F[StockService[F]] =
    F.pure(LiveStockService[F](repo, client))
