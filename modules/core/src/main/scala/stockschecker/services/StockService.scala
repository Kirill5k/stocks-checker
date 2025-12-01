package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import stockschecker.domain.{Stock, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.StockRepository

trait StockService[F[_]]:
  def findByTicker(ticker: Ticker): F[Stock]
  def findAll(limit: Option[Int]): F[List[Stock]]

final private class LiveStockService[F[_]](
    stockRepository: StockRepository[F]
)(using
    F: MonadThrow[F]
) extends StockService[F] {
  override def findByTicker(ticker: Ticker): F[Stock] =
    stockRepository.find(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))

  override def findAll(limit: Option[Int]): F[List[Stock]] =
    stockRepository.findAll(limit)
}

object StockService:
  def make[F[_]: MonadThrow](stockRepository: StockRepository[F]): F[StockService[F]] =
    MonadThrow[F].pure(LiveStockService(stockRepository))
