package stockschecker.services

import cats.effect.Concurrent
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{StockQuote, Ticker}
import stockschecker.repositories.StockRepository

trait StockService[F[_]]:
  def fetchLatest: F[Unit]
  def get(ticker: Ticker, limit: Option[Int]): F[List[StockQuote]]

final private class LiveStockService[F[_]](
    private val repository: StockRepository[F],
    private val marketDataClient: MarketDataClient[F]
)(using
    F: Concurrent[F]
) extends StockService[F] {
  override def fetchLatest: F[Unit] =
    marketDataClient.getAllTradedStocks
      .chunkN(4096)
      .evalTap(chunk => repository.save(chunk.toList))
      .compile
      .drain

  override def get(ticker: Ticker, limit: Option[Int]): F[List[StockQuote]] =
    repository.findWithPriceDeltas(ticker, limit)
}

object StockService:
  def make[F[_]](repo: StockRepository[F], client: MarketDataClient[F])(using F: Concurrent[F]): F[StockService[F]] =
    F.pure(LiveStockService[F](repo, client))
