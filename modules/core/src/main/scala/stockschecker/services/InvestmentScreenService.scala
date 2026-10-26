package stockschecker.services

import cats.effect.Sync
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.common.config.InvestmentConfig
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{
  FinancialHistoryRepository,
  InvestmentEvaluationRepository,
  InvestmentRankingRepository,
  InvestmentScreenRepository,
  JobRepository,
  StockRepository
}
import java.time.{Instant, LocalDate, ZoneOffset}
import java.nio.charset.StandardCharsets
import stockschecker.actions.Action
import java.util.UUID

trait InvestmentScreenService[F[_]]:
  def create(request: InvestmentScreenRequest): F[InvestmentScreenResult]
  def get(id: InvestmentScreenId): F[InvestmentScreenResult]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation]
  def refreshResearch: F[Unit]
  def generateScheduledScreen: F[Option[InvestmentScreenSummary]]
  def captureMonthlyEvaluations: F[Int]
  def refreshHistory(ticker: Ticker, from: LocalDate, through: LocalDate): F[Unit]

final private class LiveInvestmentScreenService[F[_]](
    private val stockRepository: StockRepository[F],
    private val financialHistory: FinancialHistoryRepository[F],
    private val screens: InvestmentScreenRepository[F],
    private val evaluations: InvestmentEvaluationRepository[F],
    private val priceHistory: EvaluationPriceHistory[F],
    private val settings: InvestmentConfig,
    private val recipe: InvestmentRecipe,
    private val rankings: InvestmentRankingRepository[F],
    private val jobs: JobRepository[F]
)(using
    F: Sync[F],
    C: Clock[F]
) extends InvestmentScreenService[F] {
  private val enabled                 = settings.enabled && settings.coverageAudited
  private def requireEnabled: F[Unit] = Sync[F].raiseUnless(enabled)(
    AppError.FailedValidation("Investment ranking is disabled until the feature is enabled and provider coverage is audited")
  )
  private def validate(request: InvestmentScreenRequest): F[Unit] =
    val thresholds = List(request.minScore, request.minQuality, request.minValue, request.minGrowth, request.minResilience).flatten
    Sync[F].raiseWhen(request.limit.exists(n => n < 1 || n > 100) || thresholds.exists(n => n < 0 || n > 100))(
      AppError.FailedValidation("limit must be between 1 and 100; score thresholds must be between 0 and 100")
    ) >>
      Sync[F].raiseWhen(request.recipeVersion.exists(_ != recipe.version))(
        AppError.FailedValidation(s"Unknown recipeVersion; supported version: ${recipe.version}")
      )
  private def load(id: InvestmentScreenId): F[InvestmentScreenRun] =
    screens.find(id).flatMap(Sync[F].fromOption(_, AppError.EntityDoesNotExist("Investment screen", id.value)))

  private def loadHeader(id: InvestmentScreenId): F[InvestmentScreenRepository.Header] =
    screens.header(id).flatMap(Sync[F].fromOption(_, AppError.EntityDoesNotExist("Investment screen", id.value)))

  private def saveScreen(
      id: InvestmentScreenId,
      request: InvestmentScreenRequest,
      ranking: InvestmentRankingRun
  ): F[InvestmentScreenResult] =
    for
      now <- Clock[F].now
      run = InvestmentScreenRun(
        id,
        now,
        ranking.recipe,
        request,
        ranking.inputs,
        ranking.scorecards,
        buildCohorts(ranking.inputs, ranking.scorecards, request),
        ranking.id
      )
      _ <- screens.save(run)
    yield result(run)

  override def create(request: InvestmentScreenRequest): F[InvestmentScreenResult] =
    for
      _         <- requireEnabled
      _         <- validate(request)
      published <- rankings.latest
      ranking   <- Sync[F].fromOption(
        published.filter(_.recipe.version == recipe.version),
        AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run")
      )
      now <- Clock[F].now
      _   <- Sync[F].raiseWhen(ranking.createdAt.isBefore(now.minusSeconds(2 * 86400)))(
        AppError.FailedValidation("The latest completed ranking is stale")
      )
      id    <- Sync[F].delay(InvestmentScreenId(UUID.randomUUID().toString))
      saved <- saveScreen(id, request, ranking)
    yield saved

  override def get(id: InvestmentScreenId): F[InvestmentScreenResult] = load(id).map(result)
  override def list(limit: Int): F[List[InvestmentScreenSummary]]     =
    Sync[F].raiseWhen(limit < 1 || limit > 100)(AppError.FailedValidation("limit must be between 1 and 100")) >> screens.list(limit)

  override def refreshHistory(ticker: Ticker, from: LocalDate, through: LocalDate): F[Unit] =
    priceHistory.history(ticker, from, through).void

  private def priceJobId(ticker: Ticker, through: LocalDate): String =
    val key = s"evaluation:${ticker.value}:${through.minusYears(13)}:$through"
    UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString

  private def pendingPriceJob(ticker: Ticker, through: LocalDate): F[Option[String]] =
    val id = priceJobId(ticker, through)
    jobs.find(id).map(_.filterNot(_.isTerminal).map(_ => id))

  private def ensurePriceJob(ticker: Ticker, through: LocalDate, now: Instant): F[Option[String]] =
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
      run: InvestmentScreenRepository.Header,
      now: Instant,
      plan: EvaluationCapturePlan,
      capture: Boolean
  ): F[InvestmentEvaluation] =
    val from    = run.createdAt.atZone(ZoneOffset.UTC).toLocalDate
    val tickers = (InvestmentEvaluator.Benchmark :: run.cohorts.flatMap(_.tickers)).distinct
    for
      gathered <- tickers.traverse(ticker => priceHistory.cached(ticker, from, plan.through).map(ticker -> _))
      histories = gathered.collect { case (ticker, Some(series)) => ticker -> series }.toMap
      report    = InvestmentEvaluator.evaluate(run._id, run.createdAt, now, run.cohorts, histories, Some(plan.through))
      absent    = gathered.collect { case (ticker, None) => ticker }
      // Retry gaps in an otherwise audited vintage too: a successful request can still omit a session.
      missing         = report.horizons.flatMap(_.cohorts.flatMap(_.missing)).filter(ticker => histories.get(ticker).exists(_.audited))
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
      run: InvestmentScreenRepository.Header,
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
      InvestmentEvaluator.evaluate(run._id, run.createdAt, now, run.cohorts, Map.empty).copy(settled = Some(false)).pure[F]
    else
      val proposed = EvaluationCapturePlan(run._id, month, through, now)
      val plan     =
        if capture then evaluations.getOrCreateCapturePlan(proposed)
        else evaluations.findCapturePlan(run._id, month).map(_.getOrElse(proposed))
      plan.flatMap(p => materializePlan(run, now, p, capture))

  override def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation] =
    for
      run      <- loadHeader(id)
      now      <- Clock[F].now
      monthly  <- evaluations.findMonthly(id, now.toString.take(7))
      finished <-
        if monthly.isEmpty && InvestmentEvaluator
            .finalCaptureThrough(run.createdAt.atZone(ZoneOffset.UTC).toLocalDate)
            .isBefore(now.atZone(ZoneOffset.UTC).toLocalDate)
        then evaluations.findFinished(id)
        else none[InvestmentEvaluation].pure[F]
      saved = monthly.orElse(finished)
      report <- saved.filter(InvestmentEvaluationRepository.settled).fold(cachedEvaluation(run, now, saved, capture = false))(_.pure[F])
    yield report

  override def refreshResearch: F[Unit] =
    if enabled then (generateScheduledScreen >> captureMonthlyEvaluations).void else Sync[F].unit

  override def generateScheduledScreen: F[Option[InvestmentScreenSummary]] =
    if !enabled then none[InvestmentScreenSummary].pure[F]
    else
      for
        now <- Clock[F].now
        date = now.atZone(ZoneOffset.UTC).toLocalDate
        recent <- rankings.latestHeader
        sameDay = recent.filter(r => r.recipe.version == recipe.version && r.createdAt.atZone(ZoneOffset.UTC).toLocalDate == date)
        header <- sameDay match
          case Some(value) => value.pure[F]
          case None        =>
            for
              stocks     <- stockRepository.findAll(StockFilters(), None)
              snapshots  <- financialHistory.findLatestAll
              capturedAt <- Clock[F].now
              id         <- Sync[F].delay(UUID.randomUUID().toString)
              byTicker   = snapshots.map(s => s.ticker -> s).toMap
              inputs     = stocks.sortBy(_.security.ticker.value).map(s => InvestmentCandidate(s, byTicker.get(s.security.ticker)))
              scorecards = InvestmentScorer.rank(inputs, capturedAt, recipe)
              run        = InvestmentRankingRun(id, capturedAt, recipe, inputs, scorecards)
              _ <- rankings.save(run)
              _ <- rankings.prune(capturedAt.minusSeconds(30L * 86400))
            yield InvestmentRankingRepository.Header(id, capturedAt, recipe, InvestmentRankingRepository.summary(run))
        monthlyId = InvestmentScreenId(s"monthly:${date.toString.take(7)}:${recipe.version}")
        existing <- screens.header(monthlyId)
        created  <- existing match
          case Some(_)                                   => none[InvestmentScreenSummary].pure[F]
          case None if header.summary.eligibleCount < 20 => none[InvestmentScreenSummary].pure[F]
          case None                                      =>
            rankings.find(header._id).flatMap {
              case Some(ranking) =>
                saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), ranking).map(r => Some(r.summary))
              case None => Sync[F].raiseError(new IllegalStateException("Published investment ranking disappeared"))
            }
      yield created

  override def captureMonthlyEvaluations: F[Int] =
    for
      now        <- Clock[F].now
      ids        <- screens.ids
      priorPlans <- evaluations.capturePlans
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
        .traverse(evaluations.getOrCreateCapturePlan)
      plans = (priorPlans ++ recoveredPlans).groupBy(p => (p.screenId, p.month)).values.map(_.head).toList
      tasks = (ids.filterNot(finished).map(id => (id, currentMonth)) ++ plans.map(p => (p.screenId, p.month))).distinct
      saved <- tasks.traverse { case (id, month) =>
        evaluations.findMonthly(id, month).flatMap {
          case Some(report) if InvestmentEvaluationRepository.settled(report) =>
            if plans.exists(p => p.screenId == id && p.month == month) then evaluations.retireCapturePlan(id, month).as(false)
            else false.pure[F]
          case existing =>
            loadHeader(id)
              .flatMap { run =>
                plans
                  .find(p => p.screenId == id && p.month == month)
                  .fold(cachedEvaluation(run, now, existing, capture = true))(materializePlan(run, now, _, capture = true))
              }
              .flatMap { evaluation =>
                if !evaluation.settled.contains(true) then false.pure[F]
                else evaluations.saveMonthly(evaluation).as(true)
              }
        }
      }
    yield saved.count(identity)

  private def inScope(input: InvestmentCandidate, request: InvestmentScreenRequest): Boolean =
    request.exchange.forall(_ == input.stock.security.exchange) &&
      request.country.forall(country => input.stock.profile.exists(_.country == country))

  private def scopedCards(run: InvestmentScreenRun): List[InvestmentScorecard] =
    val tickers = run.inputs.filter(inScope(_, run.request)).map(_.stock.security.ticker).toSet
    run.scorecards.filter(c => tickers(c.ticker))

  private def buildCohorts(
      inputs: List[InvestmentCandidate],
      cards: List[InvestmentScorecard],
      request: InvestmentScreenRequest
  ): List[InvestmentCohort] =
    val scopedInputs                                                  = inputs.filter(inScope(_, request))
    val stocks                                                        = scopedInputs.map(i => i.stock.security.ticker -> i.stock).toMap
    val eligible                                                      = cards.filter(c => c.eligible && stocks.contains(c.ticker))
    def cohort(name: String, tickers: List[Ticker]): InvestmentCohort =
      val count                                                    = tickers.size
      def weights(group: Stock => String): Map[String, BigDecimal] =
        if count == 0 then Map.empty
        else
          tickers
            .map(ticker => group(stocks(ticker)))
            .groupMapReduce(identity)(_ => 1)(_ + _)
            .view
            .mapValues(n => (BigDecimal(n) / count * 100).setScale(4, BigDecimal.RoundingMode.HALF_UP))
            .toMap
      val maxWeight  = if count == 0 then BigDecimal(0) else (BigDecimal(100) / count).setScale(4, BigDecimal.RoundingMode.HALF_UP)
      val herfindahl = if count == 0 then BigDecimal(0) else (BigDecimal(1) / count).setScale(6, BigDecimal.RoundingMode.HALF_UP)
      InvestmentCohort(
        name,
        tickers,
        Some(
          InvestmentConcentration(
            count,
            maxWeight,
            weights(s => s.profile.flatMap(p => InvestmentScorer.sectorFor(p.industry)).getOrElse("unknown")),
            weights(s => s.profile.map(_.industry).getOrElse("unknown")),
            herfindahl
          )
        )
      )
    def top(name: String, ranked: List[InvestmentScorecard]): InvestmentCohort = cohort(name, ranked.take(20).map(_.ticker))
    def byPillar(name: String): List[InvestmentScorecard]                      =
      eligible.sortBy(c => (-c.pillars.getOrElse(name, BigDecimal(0)), c.ticker.value))
    val oldPrice = eligible
      .filter(c => stocks(c.ticker).priceAnalytics.isDefined)
      .sortBy(c => (-stocks(c.ticker).priceAnalytics.get.scores.overallScore, c.ticker.value))
    List(
      top("investment", eligible.filter(c => failedCriteria(c, request).isEmpty)),
      top("old-price-score", oldPrice),
      top("quality", byPillar("quality")),
      top("value", byPillar("value")),
      cohort("equal-universe", eligible.map(_.ticker).sortBy(_.value))
    )

  private def failedCriteria(card: InvestmentScorecard, request: InvestmentScreenRequest): List[String] =
    val tests = List(
      ("score", request.minScore, card.score),
      ("quality", request.minQuality, card.pillars.get("quality")),
      ("value", request.minValue, card.pillars.get("value")),
      ("growth", request.minGrowth, card.pillars.get("growth")),
      ("resilience", request.minResilience, card.pillars.get("resilience"))
    )
    tests.flatMap { case (name, minimum, actual) =>
      minimum.filter(min => actual.forall(_ < min)).map(min => s"$name ${actual.fold("missing")(_.toString)} is below required $name $min")
    }

  private def result(run: InvestmentScreenRun): InvestmentScreenResult =
    val scoped              = scopedCards(run)
    val eligible            = scoped.filter(_.eligible)
    val assessed            = eligible.map(c => c -> failedCriteria(c, run.request))
    val matching            = assessed.collect { case (card, Nil) => card }
    val requestedNearMisses = assessed.collect { case (card, failed :: Nil) => InvestmentNearMiss(card, failed) }
    val snapshotByTicker    = run.inputs.flatMap(i => i.financialSnapshot.map(i.stock.security.ticker -> _)).toMap
    val financialNearMisses = scoped.filterNot(_.eligible).flatMap { card =>
      val explanation = card.exclusions match
        case "insufficient-cash-flow-durability" :: Nil =>
          card.evidence.map(_.financialObservations).orElse(snapshotByTicker.get(card.ticker).map(_.annual)).flatMap { observations =>
            val margins = observations.filter(_.metric == "fcfMargin").sortBy(_.period).reverse.take(5)
            Option.when(margins.size == 5)(
              s"cash-flow durability: latest annual margin ${margins.head.value}; ${margins.count(_.value > 0)}/5 positive years; requires latest > 0 and at least 4/5 positive years"
            )
          }
        case reason :: Nil if Set("nonpositive-metric:peTTM", "nonpositive-metric:pfcfShareTTM", "nonpositive-metric:epsTTM")(reason) =>
          val metric = reason.stripPrefix("nonpositive-metric:")
          snapshotByTicker.get(card.ticker).flatMap(_.metrics.get(metric)).map(value => s"$metric = $value; requires a positive value")
        case _ => None
      explanation.map(InvestmentNearMiss(card, _))
    }
    val nearMisses = (requestedNearMisses ++ financialNearMisses).take(10)
    InvestmentScreenResult(
      InvestmentScreenRepository.summary(run),
      run.request,
      matching.take(run.request.limit.getOrElse(20)),
      nearMisses,
      scoped.filterNot(_.eligible),
      run.cohorts,
      run.rankingId
    )

}

object InvestmentScreenService:
  def make[F[_]: {Sync, Clock}](
      stockRepository: StockRepository[F],
      financialHistory: FinancialHistoryRepository[F],
      screens: InvestmentScreenRepository[F],
      evaluations: InvestmentEvaluationRepository[F],
      priceHistory: EvaluationPriceHistory[F],
      settings: InvestmentConfig,
      recipe: InvestmentRecipe = InvestmentRecipe(),
      rankings: InvestmentRankingRepository[F],
      jobs: JobRepository[F]
  ): F[InvestmentScreenService[F]] =
    Sync[F].pure(
      LiveInvestmentScreenService(stockRepository, financialHistory, screens, evaluations, priceHistory, settings, recipe, rankings, jobs)
    )
