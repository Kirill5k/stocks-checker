package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import stockschecker.common.config.SecurityConfig
import stockschecker.domain.investment.InvestmentEvaluator
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{Exchange, JobId, Security, SecurityFilter, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.SecurityRepository

import java.time.Instant

trait SecurityService[F[_]]:
  def find(ticker: Ticker): F[Security]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def getAllTickers: F[List[Ticker]]
  def fetchLatest(exchanges: NonEmptyList[Exchange]): F[Unit]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]
  def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant, reason: String = "user-excluded"): F[Unit]
  def recordCompanyProfileUpdate(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LiveSecurityService[F[_]](
    private val repository: SecurityRepository[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F],
    config: SecurityConfig
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends SecurityService[F] {

  override def find(ticker: Ticker): F[Security] =
    repository.findVisible(ticker).flatMap(s => F.fromOption(s, AppError.SecurityNotFound(ticker)))

  override def findByExchange(exchange: Exchange): F[List[Security]] =
    repository.findByExchange(exchange)

  override def getAllTickers: F[List[Ticker]] =
    repository.getAllTickers

  override def fetchLatest(exchanges: NonEmptyList[Exchange]): F[Unit] =
    dispatchPendingPurges >> logger.info(s"Fetching latest securities for ${exchanges}") >>
      Stream
        .emits(exchanges.toList)
        .evalMap { exchange =>
          // Reconcile only after the provider stream has completed successfully.
          for
            securities <- marketDataClient.getTradedSecurities(exchange).compile.toList
            _ <- F.raiseWhen(securities.isEmpty)(AppError.Critical(s"Empty listing response for $exchange; reconciliation skipped"))
            listedCount <- repository.countListed(exchange)
            distinctCount = securities.map(_.ticker).distinct.size
            _ <- F.raiseWhen(listedCount > 0 && distinctCount.toDouble < listedCount * (1 - config.maxListingShrinkFraction))(
              AppError.Critical(s"Listing for $exchange shrank from $listedCount to $distinctCount; reconciliation skipped")
            )
            _ <- repository.reconcile(exchange, securities)
            _ <- dispatchPendingPurges
          yield ()
        }
        .compile
        .drain

  override def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]] =
    repository.findTickersBy(filter, limit)

  private def purge(ticker: Ticker, requestedAt: Instant): F[Unit] =
    dispatcher.dispatchOnce(JobId(s"purge-${ticker.value}-$requestedAt"), Action.PurgeStock(ticker, requestedAt)) >>
      repository.acknowledgePurge(ticker, requestedAt)

  private def dispatchPendingPurges: F[Unit] =
    for
      pending <- repository.pendingPurges
      _       <- pending.traverse_(request => purge(request.ticker, request.requestedAt))
    yield ()

  override def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant, reason: String): F[Unit] =
    F.raiseWhen(!active && ticker == InvestmentEvaluator.Benchmark)(AppError.FailedValidation(s"Cannot deactivate benchmark $ticker")) >>
      repository.setActive(ticker, active, requestedAt, reason).flatMap {
        case SecurityRepository.ActivationResult.Applied =>
          if active then
            dispatcher.dispatchOnce(
              JobId(s"reactivate-${ticker.value}-$requestedAt"),
              Action.UpdateCompanyProfiles(NonEmptyList.one(ticker))
            )
          else purge(ticker, requestedAt)
        case SecurityRepository.ActivationResult.Delisted =>
          F.raiseError(AppError.FailedValidation(s"Cannot activate delisted security $ticker"))
        case SecurityRepository.ActivationResult.Missing => F.raiseError(AppError.SecurityNotFound(ticker))
        case SecurityRepository.ActivationResult.Stale   => F.unit
      }

  override def recordCompanyProfileUpdate(tickers: NonEmptyList[Ticker]): F[Unit] =
    repository.updateCompanyProfileLastUpdated(tickers.toList)

}

object SecurityService:
  def make[F[_]](
      repository: SecurityRepository[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F],
      config: SecurityConfig = SecurityConfig(0.2)
  )(using Temporal[F], Logger[F]): F[SecurityService[F]] =
    Temporal[F].pure(LiveSecurityService[F](repository, marketDataClient, dispatcher, config))
