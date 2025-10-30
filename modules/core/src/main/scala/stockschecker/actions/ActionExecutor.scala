package stockschecker.actions

import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.applicativeError.*
import cats.syntax.foldable.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.domain.errors.AppError
import stockschecker.services.Services

import scala.concurrent.duration.*

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
        case Action.RescheduleAll                              => services.command.rescheduleAll
        case Action.Sequence(actions)                          => actions.toList.traverse_(handleAction)
        case Action.Schedule(cid, waiting)                     => F.sleep(waiting) >> services.command.execute(cid)
        case Action.FetchLatestSecurities(exchange)            => services.security.fetchLatest(exchange)
        case Action.FetchCompanyProfile(ticker)                => services.companyProfile.fetchLatest(ticker)
        case Action.FetchLatestPricePerformanceSummary(ticker) => services.price.fetchLatestPerformanceSummary(ticker)
        case Action.DiscoverSecurities(exchanges)              =>
          Stream
            .emits(exchanges.toList)
            .metered(1.second)
            .evalTap(exchange => dispatcher.dispatch(Action.FetchLatestSecurities(exchange)))
            .compile
            .drain
        case Action.EnrichCompanyProfiles(filter, limit) =>
          services.security
            .streamTickersBy(filter, limit)
            .metered(1.second)
            .evalTap(ticker => dispatcher.dispatch(Action.FetchCompanyProfile(ticker)))
            .compile
            .drain
        case Action.FetchPricePerformanceSummaries(filter, limit) =>
          services.companyProfile
            .streamTickersBy(filter, limit)
            .metered(1.second)
            .evalTap(ticker => dispatcher.dispatch(Action.FetchLatestPricePerformanceSummary(ticker)))
            .compile
            .drain
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
