package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.InvestmentScreenRepository
import java.time.{LocalDate, ZoneOffset}

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
    private val screens: InvestmentScreenRepository[F],
    private val rankings: InvestmentRankingService[F],
    private val evaluations: InvestmentEvaluationService[F],
    private val recipe: InvestmentRecipe
)(using
    F: Sync[F],
    C: Clock[F]
) extends InvestmentScreenService[F] {
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
      _       <- validate(request)
      ranking <- rankings.latest
      id      <- Sync[F].delay(InvestmentScreenId.gen)
      saved   <- saveScreen(id, request, ranking)
    yield saved

  override def get(id: InvestmentScreenId): F[InvestmentScreenResult] = load(id).map(result)
  override def list(limit: Int): F[List[InvestmentScreenSummary]]     =
    Sync[F].raiseWhen(limit < 1 || limit > 100)(AppError.FailedValidation("limit must be between 1 and 100")) >> screens.list(limit)

  override def evaluate(id: InvestmentScreenId): F[InvestmentEvaluation] = evaluations.evaluate(id)

  override def refreshHistory(ticker: Ticker, from: LocalDate, through: LocalDate): F[Unit] =
    evaluations.refreshHistory(ticker, from, through)

  override def captureMonthlyEvaluations: F[Int] = evaluations.captureMonthlyEvaluations

  override def refreshResearch: F[Unit] =
    (generateScheduledScreen >> captureMonthlyEvaluations).void

  override def generateScheduledScreen: F[Option[InvestmentScreenSummary]] =
    for
      now <- Clock[F].now
      date = now.atZone(ZoneOffset.UTC).toLocalDate
      header <- rankings.refresh(date)
      monthlyId = InvestmentScreenId(s"monthly:${date.toString.take(7)}:${recipe.version}")
      existing <- screens.header(monthlyId)
      created  <- existing match
        case Some(_)                                   => none[InvestmentScreenSummary].pure[F]
        case None if header.summary.eligibleCount < 20 => none[InvestmentScreenSummary].pure[F]
        case None                                      =>
          rankings.get(header._id).flatMap { ranking =>
            saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), ranking).map(r => Some(r.summary))
          }
    yield created

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
      screens: InvestmentScreenRepository[F],
      rankings: InvestmentRankingService[F],
      evaluations: InvestmentEvaluationService[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentScreenService[F]] =
    Sync[F].pure(LiveInvestmentScreenService(screens, rankings, evaluations, recipe))
