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

import scala.concurrent.duration.*

trait SecurityService[F[_]]:
  def find(ticker: Ticker): F[Security]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def getAllTickers: F[List[Ticker]]
  def streamAll: Stream[F, Security]
  def fetchLatest(exchanges: NonEmptyList[Exchange]): F[Unit]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]
  def recordCompanyProfileUpdate(tickers: List[Ticker]): F[Unit]

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
        .metered(1.second)
        .flatMap(exchange => marketDataClient.getTradedSecurities(exchange))
        .chunkN(512)
        .evalMap { chunk =>
          logger.info(s"Saving batch of ${chunk.size} securities") >>
            repository.save(chunk.toList)
        }
        .compile
        .drain >>
      logger.info(s"Finished fetching securities for ${exchanges}")

  override def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  override def recordCompanyProfileUpdate(tickers: List[Ticker]): F[Unit] =
    repository.updateCompanyProfileLastUpdated(tickers)
}

object SecurityService:
  def make[F[_]](
      repository: SecurityRepository[F],
      marketDataClient: MarketDataClient[F]
  )(using Temporal[F], Logger[F]): F[SecurityService[F]] =
    Temporal[F].pure(LiveSecurityService[F](repository, marketDataClient))
