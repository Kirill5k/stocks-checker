package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{Stock, StockFilters, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.StockRepository

trait StockService[F[_]]:
  def findByTicker(ticker: Ticker): F[Stock]
  def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]]
  def deactivate(ticker: Ticker): F[Unit]

final private class LiveStockService[F[_]](
    stockRepository: StockRepository[F],
    dispatcher: ActionDispatcher[F]
)(using
    F: MonadThrow[F]
) extends StockService[F] {
  override def findByTicker(ticker: Ticker): F[Stock] =
    stockRepository.find(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))

  override def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]] =
    stockRepository.findAll(filters, limit)

  override def deactivate(ticker: Ticker): F[Unit] =
    dispatcher.dispatch(Action.DeactivateSecurity(ticker)) >>
      dispatcher.dispatch(Action.DeactivateCompanyProfile(ticker))
}

object StockService:
  def make[F[_]: MonadThrow](
      stockRepository: StockRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[StockService[F]] =
    MonadThrow[F].pure(LiveStockService(stockRepository, dispatcher))
