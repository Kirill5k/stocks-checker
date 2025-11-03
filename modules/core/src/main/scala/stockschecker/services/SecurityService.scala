package stockschecker.services

import cats.{Monad, MonadThrow}
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{Exchange, Security, SecurityFilter, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.SecurityRepository

trait SecurityService[F[_]]:
  def findByTicker(ticker: Ticker): F[Security]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def getAllTickers: F[List[Ticker]]
  def streamAll: Stream[F, Security]
  def fetchLatest(exchange: Exchange): F[Unit]
  def streamTickersBy(filter: SecurityFilter, limit: Option[Int]): Stream[F, Ticker]

final private class LiveSecurityService[F[_]: {Concurrent, Logger}](
    private val repository: SecurityRepository[F],
    private val marketDataClient: MarketDataClient[F]
)(using
    F: MonadThrow[F]
) extends SecurityService[F] {

  override def findByTicker(ticker: Ticker): F[Security] =
    repository.find(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))

  override def findByExchange(exchange: Exchange): F[List[Security]] =
    repository.findByExchange(exchange)

  override def getAllTickers: F[List[Ticker]] =
    repository.getAllTickers

  override def streamAll: Stream[F, Security] =
    repository.streamAll

  override def fetchLatest(exchange: Exchange): F[Unit] =
    Logger[F].info(s"Fetching latest securities for ${exchange.fullName}") >>
      marketDataClient
        .getTradedSecurities(exchange)
        .chunkN(512)
        .evalMap { chunk =>
          Logger[F].info(s"Saving batch of ${chunk.size} securities") >>
            repository.save(chunk.toList)
        }
        .compile
        .drain >>
      Logger[F].info(s"Finished fetching securities for ${exchange.fullName}")

  override def streamTickersBy(filter: SecurityFilter, limit: Option[Int]): Stream[F, Ticker] =
    repository.streamTickersBy(filter, limit)
}

object SecurityService:
  def make[F[_]](
      repository: SecurityRepository[F],
      marketDataClient: MarketDataClient[F]
  )(using Concurrent[F], Logger[F]): F[SecurityService[F]] =
    Monad[F].pure(LiveSecurityService[F](repository, marketDataClient))
