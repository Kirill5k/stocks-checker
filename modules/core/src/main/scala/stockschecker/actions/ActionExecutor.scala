package stockschecker.actions

import cats.effect.Temporal
import cats.syntax.flatMap.*
import fs2.Stream
import org.typelevel.log4cats.Logger
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
    action match
      case Action.RescheduleAll =>
        logger.info(s"Executing ${action.kind}") >> services.command.rescheduleAll
      case Action.FetchLatestStocks =>
        logger.info(s"Executing ${action.kind}") >> services.stock.fetchLatest
      case Action.Schedule(cid, waiting) =>
        logger.info(s"Executing ${action.kind} for $cid to wait for ${waiting}") >> 
          F.sleep(waiting) >> 
          services.command.execute(cid)
}

object ActionExecutor:
  def make[F[_]: Temporal: Logger](dispatcher: ActionDispatcher[F], services: Services[F]): F[ActionExecutor[F]] =
    Temporal[F].pure(LiveActionExecutor(dispatcher, services))
