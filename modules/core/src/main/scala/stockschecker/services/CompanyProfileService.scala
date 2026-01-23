package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.CompanyProfileRepository

import scala.concurrent.duration.*

trait CompanyProfileService[F[_]]:
  def get(ticker: Ticker, fetchLatest: Boolean = false): F[CompanyProfile]
  def getAll(limit: Option[Int]): F[List[CompanyProfile]]
  def fetchLatest(tickers: NonEmptyList[Ticker]): F[Unit]
  def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]]
  def recordPriceAnalyticsUpdate(tickers: List[Ticker]): F[Unit]
  def recordFinancialMetricsUpdate(tickers: List[Ticker]): F[Unit]
  def deactivate(ticker: Ticker): F[Unit]

final private class LiveCompanyProfileService[F[_]](
    private val repository: CompanyProfileRepository[F],
    private val client: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends CompanyProfileService[F] {

  override def fetchLatest(tickers: NonEmptyList[Ticker]): F[Unit] =
    logger.info(s"Fetching latest company profiles for ${tickers.size} tickers") >>
      Stream
        .emits(tickers.toList)
        .metered(1.second)
        .evalMap { ticker =>
          fetchCompanyProfile(ticker)
            .handleErrorWith { error =>
              logger.error(error)(s"Error fetching company profile for $ticker").as(None)
            }
        }
        .unNone
        .chunkN(512)
        .evalMap { chunk =>
          logger.info(s"Saving batch of ${chunk.size} company profiles") >> save(chunk.toList)
        }
        .compile
        .drain >>
      logger.info(s"Finished fetching company profiles for ${tickers.size} tickers")

  override def get(ticker: Ticker, fetch: Boolean = false): F[CompanyProfile] = {
    val cpOpt =
      if (fetch) fetchCompanyProfile(ticker).flatTap(cp => F.whenA(cp.nonEmpty)(save(cp.toList)))
      else repository.find(ticker)

    cpOpt.flatMap(cp => F.fromOption(cp, AppError.CompanyProfileNotFound(ticker)))
  }

  override def getAll(limit: Option[Int]): F[List[CompanyProfile]] =
    repository.findAll(limit)

  private def fetchCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
    client.getCompanyProfile(ticker)

  override def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  override def recordPriceAnalyticsUpdate(tickers: List[Ticker]): F[Unit] =
    repository.updatePriceAnalyticsLastUpdated(tickers)

  private def save(cps: List[CompanyProfile]): F[Unit] =
    repository.save(cps) >> dispatcher.dispatch(Action.RecordCompanyProfileUpdate(cps.map(_.ticker)))

  override def recordFinancialMetricsUpdate(tickers: List[Ticker]): F[Unit] =
    repository.updateFinancialMetricsLastUpdated(tickers)

  override def deactivate(ticker: Ticker): F[Unit] =
    repository.deactivate(ticker)
}

object CompanyProfileService:
  def make[F[_]: {Temporal, Logger}](
      repo: CompanyProfileRepository[F],
      client: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[CompanyProfileService[F]] =
    Temporal[F].pure(LiveCompanyProfileService[F](repo, client, dispatcher))
