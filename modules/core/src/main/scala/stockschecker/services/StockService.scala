package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{Stock, StockFilters, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{CompanyProfileRepository, SecurityRepository, StockRepository}

trait StockService[F[_]]:
  def findByTicker(ticker: Ticker): F[Stock]
  def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]]
  def deactivate(ticker: Ticker): F[Unit]
  def activate(ticker: Ticker): F[Unit]

final private class LiveStockService[F[_]](
    stockRepository: StockRepository[F],
    dispatcher: ActionDispatcher[F],
    securityRepository: Option[SecurityRepository[F]],
    profileRepository: Option[CompanyProfileRepository[F]]
)(using
    F: MonadThrow[F]
) extends StockService[F] {
  override def findByTicker(ticker: Ticker): F[Stock] =
    stockRepository.find(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))

  override def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]] =
    F.fromEither(filters.validate(limit).left.map(AppError.FailedValidation(_))) >> stockRepository.findAll(filters, limit)

  override def activate(ticker: Ticker): F[Unit] =
    findByTicker(ticker).flatMap { stock =>
      securityRepository match
        case Some(repo) =>
          repo.activate(ticker) >> profileRepository.fold(F.unit)(p =>
            F.whenA(stock.security.listingStatus != "delisted")(p.activate(ticker))
          )
        case None => F.raiseError(AppError.Critical("Security state repository is not configured"))
    }

  override def deactivate(ticker: Ticker): F[Unit] =
    dispatcher.dispatch(Action.DeactivateSecurity(ticker)) >>
      dispatcher.dispatch(Action.DeactivateCompanyProfile(ticker))
}

object StockService:
  def make[F[_]: MonadThrow](
      stockRepository: StockRepository[F],
      dispatcher: ActionDispatcher[F],
      securityRepository: Option[SecurityRepository[F]] = None,
      profileRepository: Option[CompanyProfileRepository[F]] = None
  ): F[StockService[F]] =
    MonadThrow[F].pure(LiveStockService(stockRepository, dispatcher, securityRepository, profileRepository))
