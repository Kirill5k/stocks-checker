package stockschecker.actions

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.implicits.toFoldableOps
import cats.syntax.flatMap.*
import cats.syntax.applicativeError.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.domain.errors.AppError
import stockschecker.services.Services

trait ActionExecutor[F[_]]:
  def run: Stream[F, Unit]

final private class LiveActionExecutor[F[_]](
    private val dispatcher: ActionDispatcher[F],
    private val services: Services[F]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends ActionExecutor[F] {
  override def run: Stream[F, Unit] =
    dispatcher.pendingActions.map(a => Stream.eval(handleAction(a))).parJoinUnbounded

  private def handleAction(action: Action): F[Unit] =
    logger.info(s"Processing $action") >>
      (action match
        case Action.Sequence(actions)                     => actions.toList.traverse_(handleAction)
        case Action.RescheduleAll                         => services.command.rescheduleAll
        case Action.Schedule(cid, waiting)                => F.sleep(waiting) >> services.command.execute(cid)
        case Action.FetchSecurities(exchanges)            => services.security.fetchLatest(exchanges)
        case Action.UpdateCompanyProfiles(tickers)        => services.companyProfile.fetchLatest(tickers)
        case Action.UpdatePriceAnalytics(tickers)         => services.price.fetchLatestPriceAnalytics(tickers)
        case Action.UpdateFinancialMetrics(tickers)       => services.financialMetrics.fetchLatest(tickers)
        case Action.RecordPriceAnalyticsUpdate(tickers)   => services.companyProfile.recordPriceAnalyticsUpdate(tickers)
        case Action.RecordFinancialMetricsUpdate(tickers) => services.companyProfile.recordFinancialMetricsUpdate(tickers)
        case Action.RecordCompanyProfileUpdate(tickers)   => services.security.recordCompanyProfileUpdate(tickers)
        case Action.FetchCompanyProfiles(filter, limit)   =>
          services.security
            .findTickersBy(filter, limit)
            .flatMap {
              case Nil     => logger.info("Couldn't find any applicable securities for Action.FetchCompanyProfiles")
              case tickers => dispatcher.dispatch(Action.UpdateCompanyProfiles(NonEmptyList.fromListUnsafe(tickers)))
            }
        case Action.FetchPriceAnalytics(filter, limit) =>
          services.companyProfile
            .findTickersBy(filter, limit)
            .flatMap {
              case Nil     => logger.info("Couldn't find any applicable company profiles for Action.FetchPricePerformanceSummaries")
              case tickers => dispatcher.dispatch(Action.UpdatePriceAnalytics(NonEmptyList.fromListUnsafe(tickers)))
            }
        case Action.FetchFinancialMetrics(filter, limit) =>
          services.companyProfile
            .findTickersBy(filter, limit)
            .flatMap {
              case Nil     => logger.info("Couldn't find any applicable company profiles for Action.FetchFinancialMetrics")
              case tickers => dispatcher.dispatch(Action.UpdateFinancialMetrics(NonEmptyList.fromListUnsafe(tickers)))
            }
      ).handleErrorWith {
        case error: AppError =>
          logger.warn(error)(s"Domain error while processing action $action")
        case error =>
          logger.error(error)(s"Unexpected error while processing action $action")
        // TODO: add retry logic
      } >>
      logger.info(s"Finished processing $action")
}

object ActionExecutor:
  def make[F[_]](dispatcher: ActionDispatcher[F], services: Services[F])(using Temporal[F], Logger[F]): F[ActionExecutor[F]] =
    Temporal[F].pure(LiveActionExecutor(dispatcher, services))
