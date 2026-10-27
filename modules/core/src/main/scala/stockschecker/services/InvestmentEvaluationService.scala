package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import stockschecker.actions.Action
import stockschecker.domain.*
import stockschecker.utils.InvestmentEvaluator
import stockschecker.repositories.{
  EvaluationCapturePlanRepository,
  InvestmentEvaluationRepository,
  InvestmentScreenRepository,
  JobRepository
}

import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDate, ZoneOffset}
import java.util.UUID

trait InvestmentEvaluationService[F[_]]:
  def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation]
  def captureMonthlyEvaluations: F[Int]
  def refreshHistory(ticker: Ticker, range: DateRange): F[Unit]

final private class LiveInvestmentEvaluationService[F[_]](
    private val screens: InvestmentScreenRepository[F],
    private val evaluations: InvestmentEvaluationRepository[F],
    private val plans: EvaluationCapturePlanRepository[F],
    private val priceHistory: EvaluationPriceHistory[F],
    private val jobs: JobRepository[F]
)(using
    F: Sync[F],
    C: Clock[F]
) extends InvestmentEvaluationService[F] {
  override def refreshHistory(ticker: Ticker, range: DateRange): F[Unit] =
    priceHistory.history(ticker, range).void

  private def priceJobId(ticker: Ticker, through: LocalDate): JobId =
    val key = s"evaluation:${ticker.value}:${through.minusYears(13)}:$through"
    JobId(UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString)

  private def pendingPriceJob(ticker: Ticker, through: LocalDate): F[Option[JobId]] =
    val id = priceJobId(ticker, through)
    jobs.find(id).map(_.filterNot(_.isTerminal).map(_ => id))

  private def ensurePriceJob(ticker: Ticker, through: LocalDate, now: Instant): F[Option[JobId]] =
    // All screens share a ticker/cutoff job and its bounded repair budget.
    val id = priceJobId(ticker, through)
    jobs.find(id).flatMap {
      case Some(job) if job.isTerminal =>
        jobs.retryTerminal(id, now).flatMap {
          case true  => Some(id).pure[F]
          case false => pendingPriceJob(ticker, through)
        }
      case Some(_) => Some(id).pure[F]
      case None    => jobs.enqueue(Job(id, Action.RefreshEvaluationPrices(ticker, through.minusYears(13), through), now, now)).as(Some(id))
    }

  private def materializePlan(
      run: InvestmentScreenHeader,
      now: Instant,
      plan: EvaluationCapturePlan,
      capture: Boolean
  ): F[InvestmentEvaluation] =
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
      needed = (absent ++ missing ++ missingCalendar).distinct
      attempts <- needed.traverse { ticker =>
        (if capture then ensurePriceJob(ticker, plan.through, now) else pendingPriceJob(ticker, plan.through)).map(ticker -> _)
      }
      pending   = attempts.flatMap(_._2)
      exhausted = if capture then attempts.collect { case (ticker, None) => ticker.value } else Nil
      issues    = Option.when(exhausted.nonEmpty)(s"Price-history repair attempts exhausted for: ${exhausted.mkString(", ")}").toList
    yield report.copy(
      pendingDataJobs = pending,
      captureMonth = Some(plan.month),
      settled = Some(capture && pending.isEmpty),
      horizons = report.horizons.map(h => h.copy(issues = h.issues ++ issues))
    )

  private def cachedEvaluation(
      run: InvestmentScreenHeader,
      now: Instant,
      saved: Option[InvestmentEvaluation],
      capture: Boolean
  ): F[InvestmentEvaluation] =
    val from = run.createdAt.atZone(ZoneOffset.UTC).toLocalDate
    // Capture completed calendar months so different screen dates share the same provider requests.
    val monthEnd = now.atZone(ZoneOffset.UTC).toLocalDate.withDayOfMonth(1).minusDays(1)
    val through  = saved.flatMap(_.dataThrough).getOrElse(List(monthEnd, InvestmentEvaluator.finalCaptureThrough(from)).min)
    val month    = saved.flatMap(_.captureMonth).getOrElse(now.toString.take(7))
    if !through.isAfter(from) then
      InvestmentEvaluator.evaluate(run.id, run.createdAt, now, run.cohorts, Map.empty).copy(settled = Some(false)).pure[F]
    else
      val proposed = EvaluationCapturePlan(run.id, month, through, now)
      val plan     =
        if capture then plans.getOrCreate(proposed)
        else plans.find(run.id, month).map(_.getOrElse(proposed))
      plan.flatMap(p => materializePlan(run, now, p, capture))

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
      report <- saved.filter(_.isSettled).fold(cachedEvaluation(run, now, saved, capture = false))(_.pure[F])
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
                  .fold(cachedEvaluation(run, now, existing, capture = true))(materializePlan(run, now, _, capture = true))
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
      jobs: JobRepository[F]
  ): F[InvestmentEvaluationService[F]] =
    Sync[F].pure(LiveInvestmentEvaluationService(screens, evaluations, plans, priceHistory, jobs))
