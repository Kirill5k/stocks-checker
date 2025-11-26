package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import stockschecker.domain.{Stock, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.StockRepository

trait StockService[F[_]]:
  def findByTicker(ticker: Ticker): F[Stock]

final private class LiveStockService[F[_]](
    stockRepository: StockRepository[F]
)(using
    F: MonadThrow[F]
) extends StockService[F] {
  override def findByTicker(ticker: Ticker): F[Stock] =
    stockRepository.find(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))
}

object StockService:
  def make[F[_]: MonadThrow](stockRepository: StockRepository[F]): F[StockService[F]] =
    MonadThrow[F].pure(LiveStockService(stockRepository))
