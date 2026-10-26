package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{Exchange, Security, SecurityFilter, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.SecurityRepository

trait SecurityService[F[_]]:
  def find(ticker: Ticker): F[Security]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def getAllTickers: F[List[Ticker]]
  def streamAll: Stream[F, Security]
  def fetchLatest(exchanges: NonEmptyList[Exchange]): F[Unit]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]
  def recordCompanyProfileUpdate(tickers: List[Ticker]): F[Unit]
  def deactivate(ticker: Ticker): F[Unit]

final private class LiveSecurityService[F[_]](
    private val repository: SecurityRepository[F],
    private val marketDataClient: MarketDataClient[F]
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
          marketDataClient.getTradedSecurities(exchange).compile.toList.flatMap { securities =>
            if (securities.isEmpty) F.raiseError(AppError.Critical(s"Empty listing response for $exchange; reconciliation skipped"))
            else repository.reconcile(exchange, securities)
          }
        }
        .compile
        .drain

  override def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  override def recordCompanyProfileUpdate(tickers: List[Ticker]): F[Unit] =
    repository.updateCompanyProfileLastUpdated(tickers)

  override def deactivate(ticker: Ticker): F[Unit] =
    repository.deactivate(ticker)

}

object SecurityService:
  def make[F[_]](
      repository: SecurityRepository[F],
      marketDataClient: MarketDataClient[F]
  )(using Temporal[F], Logger[F]): F[SecurityService[F]] =
    Temporal[F].pure(LiveSecurityService[F](repository, marketDataClient))
