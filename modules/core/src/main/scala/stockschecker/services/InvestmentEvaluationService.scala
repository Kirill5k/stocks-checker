package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.EvaluationCapture
import stockschecker.repositories.InvestmentEvaluationRepository

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}
import java.nio.charset.StandardCharsets
import java.util.UUID

trait InvestmentEvaluationService[F[_]]:
  def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation]
  def captureMonthlyEvaluations: F[Int]

final private class LiveInvestmentEvaluationService[F[_]](
    evaluations: InvestmentEvaluationRepository[F],
    dispatcher: ActionDispatcher[F]
)(using F: Sync[F], C: Clock[F], L: Logger[F])
    extends InvestmentEvaluationService[F] {
  import InvestmentEvaluationService.historyJobId

  private def buildReport(screen: InvestmentScreen, now: Instant, month: YearMonth): F[EvaluationCapture.Draft] =
    evaluations
      .findPriceSeries(EvaluationCapture.tickers(screen), EvaluationCapture.range(screen, month))
      .map(EvaluationCapture.draft(screen, now, month, _))

  private def pending(ids: List[JobId]): F[Set[JobId]] =
    if ids.isEmpty then Set.empty[JobId].pure[F] else dispatcher.pending(ids)

  private def readCurrent(screen: InvestmentScreen, now: Instant, month: YearMonth): F[InvestmentEvaluation] =
    if !EvaluationCapture.hasCapture(screen, month) then EvaluationCapture.pending(screen, now, month).pure[F]
    else
      for
        draft <- buildReport(screen, now, month)
        jobs  <- pending(draft.needed.map(historyJobId(_, draft.through)))
      yield draft.withPending(jobs)

  override def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation] =
    for
      now <- C.now
      month = YearMonth.from(now.atZone(ZoneOffset.UTC))
      found   <- evaluations.findContext(id, month)
      context <- F.fromOption(found, AppError.EntityDoesNotExist("Investment screen", id.value))
      report  <- context.settledReport(now).fold(readCurrent(context.screen, now, month))(_.pure[F])
    yield report

  private def dispatchRequests(task: EvaluationCapture.Task, draft: EvaluationCapture.Draft): F[Unit] =
    val requested = task.existing.fold(Set.empty[Ticker])(_.requestedHistory)
    val needed    = draft.needed.filterNot(requested)
    for
      results <- needed.traverse { ticker =>
        dispatcher
          .dispatchOnce(historyJobId(ticker, draft.through), Action.RefreshEvaluationPrices(ticker, draft.through))
          .attempt
          .map(ticker -> _)
      }
      successful = results.collect { case (ticker, Right(_)) => ticker }
      // Preserve successes even if another dispatch failed, so pruned completed jobs are never recreated on resume.
      _ <- F.whenA(successful.nonEmpty)(evaluations.recordHistoryRequests(task.screen.id, task.month, successful))
      _ <- results.collectFirst { case (_, Left(error)) => error }.fold(F.unit)(F.raiseError[Unit])
    yield ()

  private def capture(task: EvaluationCapture.Task, now: Instant): F[Boolean] =
    if !EvaluationCapture.hasCapture(task.screen, task.month) then false.pure[F]
    else
      for
        draft <- buildReport(task.screen, now, task.month)
        // Persist the capture before dispatch so an interrupted run can be resumed in a later month.
        _ <- evaluations.saveMonthly(draft.report)
        _ <- dispatchRequests(task, draft)
        requests = draft.needed.map(historyJobId(_, draft.through))
        jobs <- pending(requests)
        // A worker can finish between dispatch and the pending query. Read its cached result before freezing the month.
        current <- if jobs.isEmpty && requests.nonEmpty then buildReport(task.screen, now, task.month) else draft.pure[F]
        report = current.settle(jobs)
        _ <- evaluations.saveMonthly(report)
      yield report.settled

  override def captureMonthlyEvaluations: F[Int] =
    for
      now      <- C.now
      tasks    <- evaluations.captureTasks(YearMonth.from(now.atZone(ZoneOffset.UTC)))
      captured <- tasks.traverse { task =>
        capture(task, now).attempt.flatTap {
          case Left(error) => L.error(error)(s"Monthly evaluation capture failed for screen ${task.screen.id.value}, month ${task.month}")
          case Right(_)    => F.unit
        }
      }
      _ <- captured.collectFirst { case Left(error) => error }.filter(_ => captured.forall(_.isLeft)).fold(F.unit)(F.raiseError[Unit])
    yield captured.count(_ == Right(true))
}

object InvestmentEvaluationService:
  def historyJobId(ticker: Ticker, through: LocalDate): JobId =
    val key = s"evaluation:${ticker.value}:$through"
    JobId(UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString)

  def make[F[_]: {Sync, Clock, Logger}](
      evaluations: InvestmentEvaluationRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[InvestmentEvaluationService[F]] =
    Sync[F].pure(new LiveInvestmentEvaluationService(evaluations, dispatcher))
