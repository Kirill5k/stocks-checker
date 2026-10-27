package stockschecker.services

import cats.MonadThrow
import kirill5k.common.cats.Clock
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{InvestmentCandidate, Stock, StockFilters, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.StockRepository

trait StockService[F[_]]:
  def findByTicker(ticker: Ticker): F[Stock]
  def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]]
  def findInvestmentCandidates: F[List[InvestmentCandidate]]
  def deactivate(ticker: Ticker): F[Unit]
  def activate(ticker: Ticker): F[Unit]

final private class LiveStockService[F[_]](
    stockRepository: StockRepository[F],
    dispatcher: ActionDispatcher[F]
)(using
    F: MonadThrow[F],
    clock: Clock[F]
) extends StockService[F] {
  override def findByTicker(ticker: Ticker): F[Stock] =
    stockRepository.find(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))

  override def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]] =
    F.fromEither(filters.validate(limit).left.map(AppError.FailedValidation(_))) >> stockRepository.findAll(filters, limit)

  override def findInvestmentCandidates: F[List[InvestmentCandidate]] =
    stockRepository.findInvestmentCandidates

  override def activate(ticker: Ticker): F[Unit] =
    setActive(ticker, true)

  override def deactivate(ticker: Ticker): F[Unit] =
    setActive(ticker, false)

  private def setActive(ticker: Ticker, active: Boolean): F[Unit] =
    for
      _           <- findByTicker(ticker)
      requestedAt <- clock.now
      _           <- dispatcher.dispatch(Action.SetSecurityActive(ticker, active, requestedAt))
    yield ()
}

object StockService:
  def make[F[_]: {MonadThrow, Clock}](
      stockRepository: StockRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[StockService[F]] =
    MonadThrow[F].pure(LiveStockService(stockRepository, dispatcher))
