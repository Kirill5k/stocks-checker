package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import stockschecker.actions.EvaluationPriceJobs
import stockschecker.domain.*
import stockschecker.utils.InvestmentEvaluator
import stockschecker.repositories.{EvaluationCapturePlanRepository, InvestmentEvaluationRepository, InvestmentScreenRepository}

import java.time.{Instant, ZoneOffset}

trait InvestmentEvaluationService[F[_]]:
  def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation]
  def captureMonthlyEvaluations: F[Int]
  def refreshHistory(ticker: Ticker, range: DateRange): F[Unit]

final private class LiveInvestmentEvaluationService[F[_]](
    private val screens: InvestmentScreenRepository[F],
    private val evaluations: InvestmentEvaluationRepository[F],
    private val plans: EvaluationCapturePlanRepository[F],
    private val priceHistory: EvaluationPriceHistory[F],
    private val jobs: EvaluationPriceJobs[F]
)(using
    F: Sync[F],
    C: Clock[F]
) extends InvestmentEvaluationService[F] {
  override def refreshHistory(ticker: Ticker, range: DateRange): F[Unit] =
    priceHistory.history(ticker, range).void

  private case class EvaluationDraft(report: InvestmentEvaluation, neededTickers: List[Ticker])

  private def buildReport(
      run: InvestmentScreenHeader,
      now: Instant,
      plan: EvaluationCapturePlan
  ): F[EvaluationDraft] =
    val range   = DateRange(run.createdAt.atZone(ZoneOffset.UTC).toLocalDate, plan.through)
    val tickers = (InvestmentEvaluator.Benchmark :: run.cohorts.flatMap(_.tickers)).distinct
    for
      gathered <- tickers.traverse(ticker => priceHistory.cached(ticker, range).map(ticker -> _))
      histories = gathered.collect { case (ticker, Some(series)) => ticker -> series }.toMap
      report    = InvestmentEvaluator.evaluate(run.id, run.createdAt, now, run.cohorts, histories, Some(plan.through))
      absent    = gathered.collect { case (ticker, None) => ticker }
      // Retry gaps in an otherwise audited vintage too: a successful request can still omit a session.
      missing = report.horizons
        .flatMap(_.cohorts.flatMap(_.missing))
        .filter(ticker => histories.get(ticker).exists(_.audited))
      missingCalendar = Option
        .when(
          report.horizons.exists(h => h.entryDate.isEmpty || (h.status == "incomplete" && h.exitDate.isEmpty)) &&
            histories.get(InvestmentEvaluator.Benchmark).exists(_.audited)
        )(InvestmentEvaluator.Benchmark)
        .toList
    yield EvaluationDraft(report.copy(captureMonth = Some(plan.month)), (absent ++ missing ++ missingCalendar).distinct)

  private def readPlan(run: InvestmentScreenHeader, now: Instant, plan: EvaluationCapturePlan): F[InvestmentEvaluation] =
    for
      draft   <- buildReport(run, now, plan)
      pending <- draft.neededTickers.traverse(jobs.pending(_, plan.through))
    yield draft.report.copy(pendingDataJobs = pending.flatten, settled = Some(false))

  private def capturePlan(run: InvestmentScreenHeader, now: Instant, plan: EvaluationCapturePlan): F[InvestmentEvaluation] =
    for
      draft    <- buildReport(run, now, plan)
      attempts <- draft.neededTickers.traverse { ticker =>
        jobs.ensure(ticker, plan.through, now).map(ticker -> _)
      }
      pending   = attempts.flatMap(_._2)
      exhausted = attempts.collect { case (ticker, None) => ticker.value }
      issues    = Option.when(exhausted.nonEmpty)(s"Price-history repair attempts exhausted for: ${exhausted.mkString(", ")}").toList
    yield draft.report.copy(
      pendingDataJobs = pending,
      settled = Some(pending.isEmpty),
      horizons = draft.report.horizons.map(h => h.copy(issues = h.issues ++ issues))
    )

  private def proposedPlan(
      run: InvestmentScreenHeader,
      now: Instant,
      saved: Option[InvestmentEvaluation]
  ): Option[EvaluationCapturePlan] =
    val from = run.createdAt.atZone(ZoneOffset.UTC).toLocalDate
    // Capture completed calendar months so different screen dates share the same provider requests.
    val monthEnd = now.atZone(ZoneOffset.UTC).toLocalDate.withDayOfMonth(1).minusDays(1)
    val through  = saved.flatMap(_.dataThrough).getOrElse(List(monthEnd, InvestmentEvaluator.finalCaptureThrough(from)).min)
    val month    = saved.flatMap(_.captureMonth).getOrElse(now.toString.take(7))
    Option.when(through.isAfter(from))(EvaluationCapturePlan(run.id, month, through, now))

  private def pendingEvaluation(run: InvestmentScreenHeader, now: Instant): InvestmentEvaluation =
    InvestmentEvaluator.evaluate(run.id, run.createdAt, now, run.cohorts, Map.empty).copy(settled = Some(false))

  private def readEvaluation(run: InvestmentScreenHeader, now: Instant, saved: Option[InvestmentEvaluation]): F[InvestmentEvaluation] =
    proposedPlan(run, now, saved) match
      case None           => pendingEvaluation(run, now).pure[F]
      case Some(proposed) =>
        plans.find(run.id, proposed.month).flatMap(plan => readPlan(run, now, plan.getOrElse(proposed)))

  private def captureEvaluation(run: InvestmentScreenHeader, now: Instant, saved: Option[InvestmentEvaluation]): F[InvestmentEvaluation] =
    proposedPlan(run, now, saved) match
      case None           => pendingEvaluation(run, now).pure[F]
      case Some(proposed) => plans.getOrCreate(proposed).flatMap(capturePlan(run, now, _))

  override def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation] =
    for
      run      <- screens.find(id)
      now      <- Clock[F].now
      monthly  <- evaluations.findMonthly(id, now.toString.take(7))
      finished <-
        if monthly.isEmpty && InvestmentEvaluator
            .finalCaptureThrough(run.createdAt.atZone(ZoneOffset.UTC).toLocalDate)
            .isBefore(now.atZone(ZoneOffset.UTC).toLocalDate)
        then evaluations.findFinished(id)
        else none[InvestmentEvaluation].pure[F]
      saved = monthly.orElse(finished)
      report <- saved.filter(_.isSettled).fold(readEvaluation(run, now, saved))(_.pure[F])
    yield report

  override def captureMonthlyEvaluations: F[Int] =
    for
      now        <- Clock[F].now
      ids        <- screens.ids
      priorPlans <- plans.all
      incomplete <- evaluations.unsettled
      finished   <- evaluations.finishedScreenIds
      currentMonth = now.toString.take(7)
      recovered    = incomplete.flatMap(e =>
        for
          month   <- e.captureMonth
          through <- e.dataThrough
        yield EvaluationCapturePlan(e.screenId, month, through, e.evaluatedAt)
      )
      recoveredPlans <- recovered
        .filterNot(p => priorPlans.exists(existing => existing.screenId == p.screenId && existing.month == p.month))
        .traverse(plans.getOrCreate)
      activePlans = (priorPlans ++ recoveredPlans).groupBy(p => (p.screenId, p.month)).values.map(_.head).toList
      tasks       = (ids.filterNot(finished).map(id => (id, currentMonth)) ++ activePlans.map(p => (p.screenId, p.month))).distinct
      saved <- tasks.traverse { case (id, month) =>
        evaluations.findMonthly(id, month).flatMap {
          case Some(report) if report.isSettled =>
            if activePlans.exists(p => p.screenId == id && p.month == month) then plans.retire(id, month).as(false)
            else false.pure[F]
          case existing =>
            screens
              .find(id)
              .flatMap { run =>
                activePlans
                  .find(p => p.screenId == id && p.month == month)
                  .fold(captureEvaluation(run, now, existing))(capturePlan(run, now, _))
              }
              .flatMap { evaluation =>
                if !evaluation.settled.contains(true) then false.pure[F]
                else evaluations.saveMonthly(evaluation) >> plans.retire(id, month).as(true)
              }
        }
      }
    yield saved.count(identity)

}

object InvestmentEvaluationService:
  def make[F[_]: {Sync, Clock}](
      screens: InvestmentScreenRepository[F],
      evaluations: InvestmentEvaluationRepository[F],
      plans: EvaluationCapturePlanRepository[F],
      priceHistory: EvaluationPriceHistory[F],
      jobs: EvaluationPriceJobs[F]
  ): F[InvestmentEvaluationService[F]] =
    Sync[F].pure(LiveInvestmentEvaluationService(screens, evaluations, plans, priceHistory, jobs))
