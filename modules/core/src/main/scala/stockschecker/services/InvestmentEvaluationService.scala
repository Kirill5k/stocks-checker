package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentEvaluator
import stockschecker.repositories.{InvestmentEvaluationRepository, InvestmentScreenRepository}

import java.time.{Instant, LocalDate, YearMonth, ZoneOffset}
import java.nio.charset.StandardCharsets
import java.util.UUID

trait InvestmentEvaluationService[F[_]]:
  def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation]
  def captureMonthlyEvaluations: F[Int]
  def protectedJobIds: F[Set[JobId]]
  def refreshHistory(ticker: Ticker, range: DateRange): F[Unit]

final private class LiveInvestmentEvaluationService[F[_]](
    screens: InvestmentScreenRepository[F],
    evaluations: InvestmentEvaluationRepository[F],
    priceHistory: PriceHistoryService[F],
    dispatcher: ActionDispatcher[F]
)(using F: Sync[F], C: Clock[F])
    extends InvestmentEvaluationService[F] {
  import InvestmentEvaluationService.historyJobId

  override def refreshHistory(ticker: Ticker, range: DateRange): F[Unit] =
    priceHistory.historical(ticker, range).void

  override def protectedJobIds: F[Set[JobId]] =
    evaluations.unsettled.flatMap { reports =>
      reports
        .groupBy(_.screenId)
        .toList
        .traverse { case (id, captures) =>
          screens.find(id).map { screen =>
            val tickers = (InvestmentEvaluator.Benchmark :: screen.cohorts.flatMap(_.tickers)).distinct
            val formed  = screen.createdAt.atZone(ZoneOffset.UTC).toLocalDate
            // Retain terminal siblings too: deleting one would let dispatchOnce recreate it while another remains paused.
            captures.flatMap { report =>
              report.dataThrough
                .orElse(report.captureMonth.map(InvestmentEvaluation.captureThrough(formed, _)))
                .toList
                .flatMap(through => tickers.map(historyJobId(_, through)))
            }.toSet
          }
        }
        .map(_.flatten.toSet)
    }

  private case class Draft(report: InvestmentEvaluation, needed: List[Ticker])

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
      report          = InvestmentEvaluator.evaluate(screen.id, screen.createdAt, now, screen.cohorts, histories, Some(through))
      absent          = gathered.collect { case (ticker, None) => ticker }
      missing         = report.horizons.flatMap(_.cohorts.flatMap(_.missing)).filter(ticker => histories.get(ticker).exists(_.audited))
      missingCalendar = Option
        .when(
          report.horizons.exists(h => h.entryDate.isEmpty || (h.status == EvaluationStatus.Incomplete && h.exitDate.isEmpty)) &&
            histories.get(InvestmentEvaluator.Benchmark).exists(_.audited)
        )(InvestmentEvaluator.Benchmark)
        .toList
    yield Draft(report.copy(captureMonth = Some(month)), (absent ++ missing ++ missingCalendar).distinct)

  private def pending(ids: List[JobId]): F[Set[JobId]] =
    if ids.isEmpty then Set.empty[JobId].pure[F] else dispatcher.pending(ids)

  private def hasCapture(screen: InvestmentScreen, month: YearMonth): Boolean =
    val formed = screen.createdAt.atZone(ZoneOffset.UTC).toLocalDate
    InvestmentEvaluation.captureThrough(formed, month).isAfter(formed)

  private def readCurrent(screen: InvestmentScreen, now: Instant, month: YearMonth): F[InvestmentEvaluation] =
    if !hasCapture(screen, month) then InvestmentEvaluator.evaluate(screen.id, screen.createdAt, now, screen.cohorts, Map.empty).pure[F]
    else
      for
        draft <- buildReport(screen, now, month)
        through = draft.report.dataThrough.get
        jobs <- pending(draft.needed.map(historyJobId(_, through)))
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
        through  = draft.report.dataThrough.get
        requests = draft.needed.map(ticker => ticker -> historyJobId(ticker, through))
        _ <- requests.traverse_ { case (ticker, id) =>
          dispatcher.dispatchOnce(
            id,
            Action.RefreshEvaluationPrices(ticker, through.minus(PriceHistoryService.SharedHistoryLookback), through)
          )
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
      ids        <- screens.ids
      unfinished <- evaluations.unsettled
      finished   <- evaluations.finishedScreenIds
      month = YearMonth.from(now.atZone(ZoneOffset.UTC))
      tasks = (ids.filterNot(finished).map(_ -> month) ++ unfinished.flatMap(e => e.captureMonth.map(e.screenId -> _))).distinct
      captured <- tasks.traverse { case (id, captureMonth) =>
        evaluations.findMonthly(id, captureMonth).flatMap {
          case Some(report) if report.settled => false.pure[F]
          case _                              => screens.find(id).flatMap(capture(_, now, captureMonth))
        }
      }
    yield captured.count(identity)
}

object InvestmentEvaluationService:
  def historyJobId(ticker: Ticker, through: LocalDate): JobId =
    val key = s"evaluation:${ticker.value}:${through.minus(PriceHistoryService.SharedHistoryLookback)}:$through"
    JobId(UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString)

  def make[F[_]: {Sync, Clock}](
      screens: InvestmentScreenRepository[F],
      evaluations: InvestmentEvaluationRepository[F],
      priceHistory: PriceHistoryService[F],
      dispatcher: ActionDispatcher[F]
  ): F[InvestmentEvaluationService[F]] =
    Sync[F].pure(new LiveInvestmentEvaluationService(screens, evaluations, priceHistory, dispatcher))
