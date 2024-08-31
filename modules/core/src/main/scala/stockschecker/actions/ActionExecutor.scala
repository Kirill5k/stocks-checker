package stockschecker.actions

import cats.effect.Temporal
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
        case Action.RescheduleAll =>
          services.command.rescheduleAll
        case Action.FetchLatestStocks =>
          services.stock.fetchLatest
        case Action.Schedule(cid, waiting) =>
          F.sleep(waiting) >> services.command.execute(cid)
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
  def make[F[_]: Temporal: Logger](dispatcher: ActionDispatcher[F], services: Services[F]): F[ActionExecutor[F]] =
    Temporal[F].pure(LiveActionExecutor(dispatcher, services))
