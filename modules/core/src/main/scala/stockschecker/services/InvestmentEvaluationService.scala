package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.repositories.InvestmentEvaluationRepository

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}
import java.nio.charset.StandardCharsets
import java.util.UUID

trait InvestmentEvaluationService[F[_]]:
  def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation]
  def captureMonthlyEvaluations: F[Int]
  def refreshHistory(ticker: Ticker, range: DateRange): F[Unit]

final private class LiveInvestmentEvaluationService[F[_]](
    screens: InvestmentScreenService[F],
    evaluations: InvestmentEvaluationRepository[F],
    priceHistory: PriceHistoryService[F],
    dispatcher: ActionDispatcher[F]
)(using F: Sync[F], C: Clock[F], L: Logger[F])
    extends InvestmentEvaluationService[F] {
  import InvestmentEvaluationService.historyJobId

  override def refreshHistory(ticker: Ticker, range: DateRange): F[Unit] =
    priceHistory.historical(ticker, range).void

  private case class Draft(report: InvestmentEvaluation, through: LocalDate, needed: List[Ticker])

  private def buildReport(screen: InvestmentScreen, now: Instant, month: YearMonth): F[Draft] =
    val formed  = screen.createdAt.atZone(ZoneOffset.UTC).toLocalDate
    val through = InvestmentEvaluation.captureThrough(formed, month)
    val range   = DateRange(formed, through)
    val tickers = (InvestmentEvaluator.Benchmark :: screen.cohorts.flatMap(_.tickers)).distinct
    for
      gathered <- tickers.traverse { ticker =>
        priceHistory.cachedHistorical(ticker, range).map(snapshot => ticker -> snapshot.map(EvaluationPriceSeries.from(_, range)))
      }
      histories       = gathered.collect { case (ticker, Some(series)) => ticker -> series }.toMap
      report          = InvestmentEvaluator.evaluate(screen.id, screen.createdAt, now, screen.cohorts, histories, month, through)
      absent          = gathered.collect { case (ticker, None) => ticker }
      missing         = report.horizons.flatMap(_.cohorts.flatMap(_.missing)).filter(ticker => histories.get(ticker).exists(_.audited))
      missingCalendar = Option
        .when(
          report.horizons.exists(h => h.entryDate.isEmpty || (h.status == EvaluationStatus.Incomplete && h.exitDate.isEmpty)) &&
            histories.get(InvestmentEvaluator.Benchmark).exists(_.audited)
        )(InvestmentEvaluator.Benchmark)
        .toList
    yield Draft(report, through, (absent ++ missing ++ missingCalendar).distinct)

  private def pending(ids: List[JobId]): F[Set[JobId]] =
    if ids.isEmpty then Set.empty[JobId].pure[F] else dispatcher.pending(ids)

  private def hasCapture(screen: InvestmentScreen, month: YearMonth): Boolean =
    val formed = screen.createdAt.atZone(ZoneOffset.UTC).toLocalDate
    InvestmentEvaluation.captureThrough(formed, month).isAfter(formed)

  private def readCurrent(screen: InvestmentScreen, now: Instant, month: YearMonth): F[InvestmentEvaluation] =
    if !hasCapture(screen, month) then
      val formed = screen.createdAt.atZone(ZoneOffset.UTC).toLocalDate
      InvestmentEvaluator.evaluate(screen.id, screen.createdAt, now, screen.cohorts, Map.empty, month, formed).pure[F]
    else
      for
        draft <- buildReport(screen, now, month)
        jobs  <- pending(draft.needed.map(historyJobId(_, draft.through)))
      yield draft.report.copy(pendingDataJobs = jobs.toList.sortBy(_.value))

  override def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation] =
    for
      screen <- screens.find(id)
      now    <- C.now
      month = YearMonth.from(now.atZone(ZoneOffset.UTC))
      monthly  <- evaluations.findMonthly(id, month)
      finished <-
        if monthly.isEmpty && InvestmentEvaluation
            .finalCaptureThrough(screen.createdAt.atZone(ZoneOffset.UTC).toLocalDate)
            .isBefore(now.atZone(ZoneOffset.UTC).toLocalDate)
        then evaluations.findFinished(id)
        else none[InvestmentEvaluation].pure[F]
      report <- monthly.orElse(finished).filter(_.settled).fold(readCurrent(screen, now, month))(_.pure[F])
    yield report

  private def capture(screen: InvestmentScreen, now: Instant, month: YearMonth): F[Boolean] =
    if !hasCapture(screen, month) then false.pure[F]
    else
      for
        draft <- buildReport(screen, now, month)
        // Persist the capture before dispatch so an interrupted run can be resumed in a later month.
        _ <- evaluations.saveMonthly(draft.report)
        through  = draft.through
        requests = draft.needed.map(ticker => ticker -> historyJobId(ticker, through))
        requested <- evaluations.requestedHistory(screen.id, month)
        _         <- requests.filterNot { case (ticker, _) => requested(ticker) }.traverse_ { case (ticker, id) =>
          dispatcher.dispatchOnce(
            id,
            Action.RefreshEvaluationPrices(ticker, through)
          ) >> evaluations.recordHistoryRequest(screen.id, month, ticker)
        }
        jobs <- pending(requests.map(_._2))
        // A worker can finish between dispatch and the pending query. Read its cached result before freezing the month.
        current <- if jobs.isEmpty && requests.nonEmpty then buildReport(screen, now, month) else draft.pure[F]
        unresolved = Option.when(jobs.isEmpty)(current.needed.map(_.value)).getOrElse(Nil)
        issues     = Option
          .when(unresolved.nonEmpty)(s"Price history remains unavailable after completed capture jobs for: ${unresolved.mkString(", ")}")
          .toList
        report = current.report.copy(
          pendingDataJobs = jobs.toList.sortBy(_.value),
          settled = jobs.isEmpty,
          horizons = current.report.horizons.map(h => h.copy(issues = h.issues ++ issues))
        )
        _ <- evaluations.saveMonthly(report)
      yield report.settled

  override def captureMonthlyEvaluations: F[Int] =
    for
      now        <- C.now
      allScreens <- screens.all
      unfinished <- evaluations.unsettled
      finished   <- evaluations.finishedScreenIds
      month = YearMonth.from(now.atZone(ZoneOffset.UTC))
      byId  = allScreens.map(screen => screen.id -> screen).toMap
      tasks = (allScreens.map(_.id).filterNot(finished).map(_ -> month) ++ unfinished.map(e => e.screenId -> e.captureMonth)).distinct
      captured <- tasks.traverse { case (id, captureMonth) =>
        evaluations
          .findMonthly(id, captureMonth)
          .flatMap {
            case Some(report) if report.settled => false.pure[F]
            case _                              =>
              F.fromOption(byId.get(id), AppError.EntityDoesNotExist("Investment screen", id.value)).flatMap(capture(_, now, captureMonth))
          }
          .attempt
          .flatTap {
            case Left(error) => L.error(error)(s"Monthly evaluation capture failed for screen ${id.value}, month $captureMonth")
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
      screens: InvestmentScreenService[F],
      evaluations: InvestmentEvaluationRepository[F],
      priceHistory: PriceHistoryService[F],
      dispatcher: ActionDispatcher[F]
  ): F[InvestmentEvaluationService[F]] =
    Sync[F].pure(new LiveInvestmentEvaluationService(screens, evaluations, priceHistory, dispatcher))
