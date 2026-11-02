package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.CompanyProfileRepository

trait CompanyProfileService[F[_]]:
  def get(ticker: Ticker, fetchLatest: Boolean = false): F[CompanyProfile]
  def getAll(limit: Option[Int]): F[List[CompanyProfile]]
  def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]]
  def delete(ticker: Ticker): F[Unit]
  def recordPriceAnalyticsUpdate(tickers: NonEmptyList[Ticker]): F[Unit]
  def recordFinancialMetricsUpdate(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LiveCompanyProfileService[F[_]](
    private val repository: CompanyProfileRepository[F],
    private val client: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Sync[F],
    C: Clock[F]
) extends CompanyProfileService[F] {

  override def get(ticker: Ticker, fetch: Boolean = false): F[CompanyProfile] =
    if fetch then fetchCompanyProfile(ticker)
    else repository.find(ticker).flatMap(F.fromOption(_, AppError.CompanyProfileNotFound(ticker)))

  private def fetchCompanyProfile(ticker: Ticker): F[CompanyProfile] =
    for
      requestedAt <- C.now
      profile     <- client.getCompanyProfile(ticker)
      _           <- profile match
        case Some(cp) => repository.save(cp) >> dispatcher.dispatch(Action.RecordCompanyProfileUpdate(NonEmptyList.one(ticker)))
        case None     => dispatcher.dispatch(Action.SetSecurityActive(ticker, false, requestedAt, "provider-profile-empty"))
      result <- F.fromOption(profile, AppError.CompanyProfileNotFound(ticker))
    yield result

  override def getAll(limit: Option[Int]): F[List[CompanyProfile]] =
    repository.findAll(limit)

  override def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int] = None): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  override def delete(ticker: Ticker): F[Unit] =
    repository.delete(ticker)

  override def recordPriceAnalyticsUpdate(tickers: NonEmptyList[Ticker]): F[Unit] =
    repository.updatePriceAnalyticsLastUpdated(tickers.toList)

  override def recordFinancialMetricsUpdate(tickers: NonEmptyList[Ticker]): F[Unit] =
    repository.updateFinancialMetricsLastUpdated(tickers.toList)
}

object CompanyProfileService:
  def make[F[_]: {Sync, Clock}](
      repo: CompanyProfileRepository[F],
      client: MarketDataClient[F],
      dispatcher: ActionDispatcher[F]
  ): F[CompanyProfileService[F]] =
    Sync[F].pure(LiveCompanyProfileService[F](repo, client, dispatcher))
