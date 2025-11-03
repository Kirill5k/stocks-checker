package stockschecker.services

import cats.effect.Concurrent
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
  def fetchLatest(exchange: Exchange): F[Unit]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]

final private class LiveSecurityService[F[_]](
    private val repository: SecurityRepository[F],
    private val marketDataClient: MarketDataClient[F]
)(using
    F: Concurrent[F],
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

  override def fetchLatest(exchange: Exchange): F[Unit] =
    logger.info(s"Fetching latest securities for ${exchange.fullName}") >>
      marketDataClient
        .getTradedSecurities(exchange)
        .chunkN(512)
        .evalMap { chunk =>
          logger.info(s"Saving batch of ${chunk.size} securities") >>
            repository.save(chunk.toList)
        }
        .compile
        .drain >>
      logger.info(s"Finished fetching securities for ${exchange.fullName}")

  override def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)
}

object SecurityService:
  def make[F[_]](
      repository: SecurityRepository[F],
      marketDataClient: MarketDataClient[F]
  )(using Concurrent[F], Logger[F]): F[SecurityService[F]] =
    Concurrent[F].pure(LiveSecurityService[F](repository, marketDataClient))
