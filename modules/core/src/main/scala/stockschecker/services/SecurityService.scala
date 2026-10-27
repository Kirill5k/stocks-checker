package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{Exchange, ListingStatus, Security, SecurityFilter, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.SecurityRepository

import java.time.Instant

trait SecurityService[F[_]]:
  def find(ticker: Ticker): F[Security]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def getAllTickers: F[List[Ticker]]
  def streamAll: Stream[F, Security]
  def fetchLatest(exchanges: NonEmptyList[Exchange]): F[Unit]
  def findExcluded: F[List[Security]]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]
  def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant): F[Unit]
  def recordCompanyProfileUpdate(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LiveSecurityService[F[_]](
    private val repository: SecurityRepository[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends SecurityService[F] {

  override def find(ticker: Ticker): F[Security] =
    repository.find(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))

  override def findByExchange(exchange: Exchange): F[List[Security]] =
    repository.findByExchange(exchange)

  override def getAllTickers: F[List[Ticker]] =
    repository.getAllTickers

  override def streamAll: Stream[F, Security] =
    repository.streamAll

  override def fetchLatest(exchanges: NonEmptyList[Exchange]): F[Unit] =
    logger.info(s"Fetching latest securities for ${exchanges}") >>
      Stream
        .emits(exchanges.toList)
        .evalMap { exchange =>
          // Reconcile only after the provider stream has completed successfully.
          for
            securities <- marketDataClient.getTradedSecurities(exchange).compile.toList
            _ <- F.raiseWhen(securities.isEmpty)(AppError.Critical(s"Empty listing response for $exchange; reconciliation skipped"))
            _ <- repository.reconcile(exchange, securities)
          yield ()
        }
        .compile
        .drain

  override def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  override def findExcluded: F[List[Security]] =
    repository.findExcluded

  override def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant): F[Unit] =
    for
      security <- find(ticker)
      _        <- repository.setActive(ticker, active, requestedAt)
      action = Action.SetCompanyProfileActive(ticker, active && security.listingStatus != ListingStatus.Delisted, requestedAt)
      _ <- dispatcher.dispatch(action)
    yield ()

  override def recordCompanyProfileUpdate(tickers: NonEmptyList[Ticker]): F[Unit] =
    repository.updateCompanyProfileLastUpdated(tickers.toList)

}

object SecurityService:
  def make[F[_]](
      repository: SecurityRepository[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  )(using Temporal[F], Logger[F]): F[SecurityService[F]] =
    Temporal[F].pure(LiveSecurityService[F](repository, marketDataClient, dispatcher))
