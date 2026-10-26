package stockschecker.services

import cats.effect.Sync
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{FinancialHistoryRepository, InvestmentEvaluationRepository, InvestmentScreenRepository, InvestmentRankingRepository, JobRepository, StockRepository}
import java.time.{Instant, LocalDate, ZoneOffset}
import java.nio.charset.StandardCharsets
import stockschecker.actions.Action
import java.util.UUID

trait InvestmentScreenService[F[_]]:
  def create(request: InvestmentScreenRequest): F[InvestmentScreenResult]
  def get(id: String): F[InvestmentScreenResult]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def evaluate(id: String): F[InvestmentEvaluation]
  def generateScheduledScreen: F[Option[InvestmentScreenSummary]]
  def captureMonthlyEvaluations: F[Int]
  def refreshHistory(ticker: Ticker, from: LocalDate, through: LocalDate): F[Unit]

object InvestmentScreenService:
  def make[F[_]: {Sync, Clock}](
      stockRepository: StockRepository[F],
      financialHistory: FinancialHistoryRepository[F],
      screens: InvestmentScreenRepository[F],
      evaluations: InvestmentEvaluationRepository[F],
      priceHistory: EvaluationPriceHistory[F],
      settings: InvestmentScreenSettings,
      recipe: InvestmentRecipe = InvestmentRecipe(),
      rankings: InvestmentRankingRepository[F],
      jobs: Option[JobRepository[F]] = None
  ): F[InvestmentScreenService[F]] = Sync[F].pure(new InvestmentScreenService[F]:
    private val enabled = settings.enabled && settings.coverageAudited
    private def requireEnabled: F[Unit] = Sync[F].raiseUnless(enabled)(
      AppError.FailedValidation("Investment ranking is disabled until the feature is enabled and provider coverage is audited")
    )
    private def validate(request: InvestmentScreenRequest): F[Unit] =
      val thresholds = List(request.minScore, request.minQuality, request.minValue, request.minGrowth, request.minResilience).flatten
      Sync[F].raiseWhen(request.limit.exists(n => n < 1 || n > 100) || thresholds.exists(n => n < 0 || n > 100))(
        AppError.FailedValidation("limit must be between 1 and 100; score thresholds must be between 0 and 100")) >>
        Sync[F].raiseWhen(request.recipeVersion.exists(_ != recipe.version))(
          AppError.FailedValidation(s"Unknown recipeVersion; supported version: ${recipe.version}"))
    private def load(id: String): F[InvestmentScreenRun] =
      screens.find(id).flatMap(Sync[F].fromOption(_, AppError.EntityDoesNotExist("Investment screen", id)))

    private def saveScreen(id: String, request: InvestmentScreenRequest, ranking: InvestmentRankingRun): F[InvestmentScreenResult] =
      for
        now <- Clock[F].now
        run = InvestmentScreenRun(id, now, ranking.recipe, request, ranking.inputs, ranking.scorecards,
          buildCohorts(ranking.inputs, ranking.scorecards, request), ranking.id)
        _ <- screens.save(run)
      yield result(run)

    def create(request: InvestmentScreenRequest): F[InvestmentScreenResult] =
      for
        _ <- requireEnabled
        _ <- validate(request)
        published <- rankings.latest
        ranking <- Sync[F].fromOption(published.filter(_.recipe.version == recipe.version),
          AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run"))
        now <- Clock[F].now
        _ <- Sync[F].raiseWhen(ranking.createdAt.isBefore(now.minusSeconds(2 * 86400)))(
          AppError.FailedValidation("The latest completed ranking is stale"))
        id <- Sync[F].delay(UUID.randomUUID().toString)
        saved <- saveScreen(id, request, ranking)
      yield saved

    def get(id: String): F[InvestmentScreenResult] = load(id).map(result)
    def list(limit: Int): F[List[InvestmentScreenSummary]] =
      Sync[F].raiseWhen(limit < 1 || limit > 100)(AppError.FailedValidation("limit must be between 1 and 100")) >> screens.list(limit)

    def refreshHistory(ticker: Ticker, from: LocalDate, through: LocalDate): F[Unit] =
      priceHistory.history(ticker, from, through).void

    private def ensurePriceJob(repository: JobRepository[F], ticker: Ticker, from: LocalDate, through: LocalDate, now: Instant): F[Option[String]] =
      val key = s"evaluation:${now.toString.take(7)}:${ticker.value}:$from:$through"
      val id = UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString
      repository.find(id).flatMap {
        case Some(job) => Option.when(!job.isTerminal)(id).pure[F]
        case None => repository.enqueue(Job(id, Action.RefreshEvaluationPrices(ticker, from, through), now, now)).as(Some(id))
      }

    private def materializePlan(run: InvestmentScreenRun, now: Instant, repository: JobRepository[F], plan: EvaluationCapturePlan): F[InvestmentEvaluation] =
      val from = run.createdAt.atZone(ZoneOffset.UTC).toLocalDate
      val tickers = (InvestmentEvaluator.Benchmark :: run.cohorts.flatMap(_.tickers)).distinct
      tickers.traverse { ticker =>
        priceHistory.cached(ticker, from, plan.through).flatMap {
          case Some(series) => (Some(ticker -> series), Option.empty[String]).pure[F]
          case None => ensurePriceJob(repository, ticker, from, plan.through, plan.createdAt).map(pending => (None, pending))
        }
      }.map { gathered =>
        InvestmentEvaluator.evaluate(run.id, run.createdAt, now, run.cohorts, gathered.flatMap(_._1).toMap, Some(plan.through))
          .copy(pendingDataJobs = gathered.flatMap(_._2), captureMonth = Some(plan.month))
      }

    private def cachedEvaluation(run: InvestmentScreenRun, now: Instant, repository: JobRepository[F]): F[InvestmentEvaluation] =
      val from = run.createdAt.atZone(ZoneOffset.UTC).toLocalDate
      val today = now.atZone(ZoneOffset.UTC).toLocalDate
      val through = List(today.minusDays(1), from.plusYears(5).plusDays(14)).min
      val month = now.toString.take(7)
      if !through.isAfter(from) then InvestmentEvaluator.evaluate(run.id, run.createdAt, now, run.cohorts, Map.empty).pure[F]
      else evaluations.findCapturePlan(run.id, month).flatMap {
        case Some(plan) => materializePlan(run, now, repository, plan)
        case None =>
          // First discover the benchmark entry. A weekend/holiday cannot lock the month to an empty calendar.
          priceHistory.cached(InvestmentEvaluator.Benchmark, from, through).flatMap {
            case Some(benchmark) if benchmark.closes.exists(c => c.date.isAfter(from) && !c.date.isAfter(through)) =>
              evaluations.getOrCreateCapturePlan(EvaluationCapturePlan(run.id, month, through, now)).flatMap(plan => materializePlan(run, now, repository, plan))
            case _ => ensurePriceJob(repository, InvestmentEvaluator.Benchmark, from, through, now).map { pending =>
              val report = InvestmentEvaluator.evaluate(run.id, run.createdAt, now, run.cohorts, Map.empty)
              report.copy(pendingDataJobs = pending.toList, dataThrough = Option.when(pending.isEmpty)(through), captureMonth = Some(month),
                horizons = report.horizons.map(h => h.copy(issues = h.issues ++ Option.when(pending.isEmpty)("Benchmark entry unavailable after terminal price-history job"))))
            }
          }
      }

    def evaluate(id: String): F[InvestmentEvaluation] =
      for
        run <- load(id)
        now <- Clock[F].now
        report <- jobs match
          case Some(repository) => evaluations.findMonthly(id, now.toString.take(7)).flatMap {
            case Some(saved) => saved.pure[F]
            case None => cachedEvaluation(run, now, repository)
          }
          case None =>
            val from = run.createdAt.atZone(ZoneOffset.UTC).toLocalDate
            val today = now.atZone(ZoneOffset.UTC).toLocalDate
            val through = List(today.minusDays(1), from.plusYears(5).plusDays(14)).min
            val tickers = if !through.isAfter(from) then Nil else (InvestmentEvaluator.Benchmark :: run.cohorts.flatMap(_.tickers)).distinct
            tickers.traverse { ticker =>
              priceHistory.history(ticker, from, through).handleError { error =>
                EvaluationPriceSeries(ticker, Nil, "unavailable", now, false, "unknown", from, from,
                  List(s"history fetch failed: ${error.getClass.getSimpleName}"))
              }.map(ticker -> _)
            }.map(histories => InvestmentEvaluator.evaluate(run.id, run.createdAt, now, run.cohorts, histories.toMap))
      yield report

    def generateScheduledScreen: F[Option[InvestmentScreenSummary]] =
      if !enabled then none[InvestmentScreenSummary].pure[F]
      else
        for
          now <- Clock[F].now
          date = now.atZone(ZoneOffset.UTC).toLocalDate
          recent <- rankings.latest
          sameDay = recent.filter(r => r.recipe.version == recipe.version && r.createdAt.atZone(ZoneOffset.UTC).toLocalDate == date)
          ranking <- sameDay match
            case Some(run) => run.pure[F]
            case None =>
              for
                stocks <- stockRepository.findAll(StockFilters(), None)
                snapshots <- financialHistory.findLatestAll
                capturedAt <- Clock[F].now
                id <- Sync[F].delay(UUID.randomUUID().toString)
                // The immutable financial-history document retains the raw response. Ranking inputs
                // retain its id/hash and all mapped observations without duplicating the raw JSON daily.
                byTicker = snapshots.map(s => s.ticker -> s.copy(rawPayload = "")).toMap
                inputs = stocks.sortBy(_.security.ticker.value).map(s => InvestmentCandidate(s, byTicker.get(s.security.ticker)))
                scorecards = InvestmentScorer.rank(inputs, capturedAt, recipe)
                run = InvestmentRankingRun(id, capturedAt, recipe, inputs, scorecards)
                _ <- rankings.save(run)
              yield run
          monthlyId = s"monthly:${date.toString.take(7)}:${recipe.version}"
          existing <- screens.find(monthlyId)
          created <- existing match
            case Some(_) => none[InvestmentScreenSummary].pure[F]
            case None if ranking.scorecards.count(_.eligible) < 20 => none[InvestmentScreenSummary].pure[F]
            case None => saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), ranking).map(r => Some(r.summary))
        yield created

    def captureMonthlyEvaluations: F[Int] =
      for
        now <- Clock[F].now
        ids <- screens.ids
        priorPlans <- if jobs.isDefined then evaluations.capturePlans else List.empty[EvaluationCapturePlan].pure[F]
        currentMonth = now.toString.take(7)
        tasks = ids.map(id => (id, currentMonth, Option.empty[EvaluationCapturePlan])) ++
          priorPlans.filter(_.month != currentMonth).map(plan => (plan.screenId, plan.month, Some(plan)))
        saved <- tasks.traverse { case (id, month, plan) =>
          evaluations.findMonthly(id, month).flatMap {
            case Some(_) => false.pure[F]
            case None =>
              val calculation = (plan, jobs) match
                case (Some(p), Some(repository)) => load(id).flatMap(run => materializePlan(run, now, repository, p))
                case _ => evaluate(id)
              calculation.flatMap { evaluation =>
                if evaluation.pendingDataJobs.nonEmpty || (evaluation.dataThrough.isEmpty && evaluation.horizons.forall(_.entryDate.isEmpty)) then false.pure[F]
                else evaluations.saveMonthly(evaluation).as(true)
              }
          }
        }
      yield saved.count(identity)

  )

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
    val scopedInputs = inputs.filter(inScope(_, request))
    val stocks = scopedInputs.map(i => i.stock.security.ticker -> i.stock).toMap
    val eligible = cards.filter(c => c.eligible && stocks.contains(c.ticker))
    def cohort(name: String, tickers: List[Ticker]): InvestmentCohort =
      val count = tickers.size
      def weights(group: Stock => String): Map[String, BigDecimal] =
        if count == 0 then Map.empty
        else tickers.map(ticker => group(stocks(ticker))).groupMapReduce(identity)(_ => 1)(_ + _)
          .view.mapValues(n => (BigDecimal(n) / count * 100).setScale(4, BigDecimal.RoundingMode.HALF_UP)).toMap
      val maxWeight = if count == 0 then BigDecimal(0) else (BigDecimal(100) / count).setScale(4, BigDecimal.RoundingMode.HALF_UP)
      val herfindahl = if count == 0 then BigDecimal(0) else (BigDecimal(1) / count).setScale(6, BigDecimal.RoundingMode.HALF_UP)
      InvestmentCohort(name, tickers, Some(InvestmentConcentration(count, maxWeight,
        weights(s => s.profile.flatMap(p => InvestmentScorer.sectorFor(p.industry)).getOrElse("unknown")),
        weights(s => s.profile.map(_.industry).getOrElse("unknown")), herfindahl)))
    def top(name: String, ranked: List[InvestmentScorecard]): InvestmentCohort = cohort(name, ranked.take(20).map(_.ticker))
    def byPillar(name: String): List[InvestmentScorecard] =
      eligible.sortBy(c => (-c.pillars.getOrElse(name, BigDecimal(0)), c.ticker.value))
    val oldPrice = eligible.filter(c => stocks(c.ticker).priceAnalytics.isDefined)
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
      ("resilience", request.minResilience, card.pillars.get("resilience")))
    tests.flatMap { case (name, minimum, actual) =>
      minimum.filter(min => actual.forall(_ < min)).map(min => s"$name ${actual.fold("missing")(_.toString)} is below required $name $min")
    }

  private def result(run: InvestmentScreenRun): InvestmentScreenResult =
    val scoped = scopedCards(run)
    val eligible = scoped.filter(_.eligible)
    val assessed = eligible.map(c => c -> failedCriteria(c, run.request))
    val matching = assessed.collect { case (card, Nil) => card }
    val requestedNearMisses = assessed.collect { case (card, failed :: Nil) => InvestmentNearMiss(card, failed) }
    val snapshotByTicker = run.inputs.flatMap(i => i.financialSnapshot.map(i.stock.security.ticker -> _)).toMap
    val financialNearMisses = scoped.filterNot(_.eligible).flatMap { card =>
      val explanation = card.exclusions match
        case "insufficient-cash-flow-durability" :: Nil =>
          snapshotByTicker.get(card.ticker).flatMap { snapshot =>
            val margins = snapshot.annual.filter(_.metric == "fcfMargin").sortBy(_.period).reverse.take(5)
            Option.when(margins.size == 5)(s"cash-flow durability: latest annual margin ${margins.head.value}; ${margins.count(_.value > 0)}/5 positive years; requires latest > 0 and at least 4/5 positive years")
          }
        case reason :: Nil if Set("nonpositive-metric:peTTM", "nonpositive-metric:pfcfShareTTM", "nonpositive-metric:epsTTM")(reason) =>
          val metric = reason.stripPrefix("nonpositive-metric:")
          snapshotByTicker.get(card.ticker).flatMap(_.metrics.get(metric)).map(value => s"$metric = $value; requires a positive value")
        case _ => None
      explanation.map(InvestmentNearMiss(card, _))
    }
    val nearMisses = (requestedNearMisses ++ financialNearMisses).take(10)
    InvestmentScreenResult(InvestmentScreenRepository.summary(run), run.request, matching.take(run.request.limit.getOrElse(20)), nearMisses,
      scoped.filterNot(_.eligible), run.cohorts, run.rankingId)
