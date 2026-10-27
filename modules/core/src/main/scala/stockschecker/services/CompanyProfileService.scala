package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.CompanyProfileRepository

import java.time.Instant

trait CompanyProfileService[F[_]]:
  def get(ticker: Ticker, fetchLatest: Boolean = false): F[CompanyProfile]
  def getAll(limit: Option[Int]): F[List[CompanyProfile]]
  def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]]
  def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant): F[Unit]
  def recordPriceAnalyticsUpdate(tickers: NonEmptyList[Ticker]): F[Unit]
  def recordFinancialMetricsUpdate(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LiveCompanyProfileService[F[_]](
    private val repository: CompanyProfileRepository[F],
    private val client: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Sync[F]
) extends CompanyProfileService[F] {

  override def get(ticker: Ticker, fetch: Boolean = false): F[CompanyProfile] = {
    val cpOpt =
      if (fetch) fetchCompanyProfile(ticker).flatTap(_.traverse_(cp => save(NonEmptyList.one(cp))))
      else repository.find(ticker)

    cpOpt.flatMap(cp => F.fromOption(cp, AppError.CompanyProfileNotFound(ticker)))
  }

  override def getAll(limit: Option[Int]): F[List[CompanyProfile]] =
    repository.findAll(limit)

  private def fetchCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
    client.getCompanyProfile(ticker)

  override def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  private def save(cps: NonEmptyList[CompanyProfile]): F[Unit] =
    repository.save(cps.toList) >> dispatcher.dispatch(Action.RecordCompanyProfileUpdate(cps.map(_.ticker)))

  override def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant): F[Unit] =
    repository.setActive(ticker, active, requestedAt)

  override def recordPriceAnalyticsUpdate(tickers: NonEmptyList[Ticker]): F[Unit] =
    repository.updatePriceAnalyticsLastUpdated(tickers.toList)

  override def recordFinancialMetricsUpdate(tickers: NonEmptyList[Ticker]): F[Unit] =
    repository.updateFinancialMetricsLastUpdated(tickers.toList)
}

object CompanyProfileService:
  def make[F[_]: Sync](
      repo: CompanyProfileRepository[F],
      client: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[CompanyProfileService[F]] =
    Sync[F].pure(LiveCompanyProfileService[F](repo, client, dispatcher))
