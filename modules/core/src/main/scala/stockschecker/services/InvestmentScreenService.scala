package stockschecker.services

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.{InvestmentReasons, InvestmentScreenSelection}
import stockschecker.repositories.InvestmentScreenRepository

import java.time.{Instant, YearMonth, ZoneOffset}

trait InvestmentScreenService[F[_]]:
  def create(request: InvestmentScreenRequest): F[InvestmentScreenResult]
  def get(id: InvestmentScreenId): F[InvestmentScreenResult]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def createMonthlyScreen: F[MonthlyScreenOutcome]

final private class LiveInvestmentScreenService[F[_]](
    private val screens: InvestmentScreenRepository[F],
    private val recipe: InvestmentRecipe
)(using
    F: Sync[F],
    C: Clock[F],
    logger: Logger[F]
) extends InvestmentScreenService[F] {
  private def currentEntries(id: InvestmentRankingId, requireScoringPeers: Boolean): F[List[InvestmentRankingEntry]] =
    for
      entries <- screens.rankingEntries(id)
      visible <- screens.visibleRankingTickers(id)
      peers = entries.filter(entry =>
        entry.scorecard.eligible || entry.scorecard.exclusions == List(InvestmentReasons.InsufficientPeerCoverage)
      )
      _ <- F.raiseWhen(requireScoringPeers && peers.exists(entry => !visible(entry.scorecard.ticker)))(
        AppError.FailedValidation(
          "The ranking contains scoring peers that are no longer active; refresh the ranking before creating a screen"
        )
      )
    yield entries.filter(entry => visible(entry.scorecard.ticker))

  private def saveScreen(
      id: InvestmentScreenId,
      request: InvestmentScreenRequest,
      ranking: InvestmentRanking
  ): F[InvestmentScreenResult] =
    for
      entries <- currentEntries(ranking.id, requireScoringPeers = true)
      now     <- C.now
      selection = InvestmentScreenSelection(entries, request)
      screen    = InvestmentScreen.create(id, now, ranking, request, selection.cohorts)
      _ <- screens.save(screen)
    yield selection.result(screen)

  override def create(request: InvestmentScreenRequest): F[InvestmentScreenResult] =
    for
      _       <- F.fromEither(request.validate(recipe).left.map(AppError.FailedValidation(_)))
      now     <- C.now
      ranking <- latestRanking(now)
      id      <- F.delay(InvestmentScreenId.gen)
      saved   <- saveScreen(id, request, ranking)
    yield saved

  override def get(id: InvestmentScreenId): F[InvestmentScreenResult] =
    for
      context <- screens.findWithRanking(id)
      (screen, ranking) = context
      entries <- currentEntries(ranking.id, requireScoringPeers = false)
      visible = entries.iterator.map(_.scorecard.ticker).toSet
      _ <- F.raiseWhen(screen.cohorts.iterator.flatMap(_.tickers).exists(ticker => !visible(ticker)))(
        AppError.EntityDoesNotExist("Investment screen", id.value)
      )
    yield InvestmentScreenSelection(entries, screen.request).result(screen)

  private def latestRanking(now: Instant): F[InvestmentRanking] =
    for
      published <- screens.latestRanking
      ranking   <- F.fromOption(
        published,
        AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run")
      )
      usable <- F.fromEither(ranking.requireUsable(recipe, now).left.map(AppError.FailedValidation(_)))
    yield usable

  override def list(limit: Int): F[List[InvestmentScreenSummary]] =
    F.raiseWhen(limit < InvestmentRecipe.MinimumLimit || limit > InvestmentRecipe.MaximumLimit)(
      AppError.FailedValidation(s"limit must be between ${InvestmentRecipe.MinimumLimit} and ${InvestmentRecipe.MaximumLimit}")
    ) >> screens.list(limit)

  override def createMonthlyScreen: F[MonthlyScreenOutcome] =
    for
      now <- C.now
      date      = now.atZone(ZoneOffset.UTC).toLocalDate
      monthlyId = InvestmentScreenId(s"monthly:${YearMonth.from(date)}:${recipe.version}")
      existing <- screens.get(monthlyId)
      outcome  <- existing match
        case Some(screen) =>
          F.pure[MonthlyScreenOutcome](MonthlyScreenOutcome.AlreadyExists(screen.rankingId, screen.summary))
        case None =>
          for
            ranking <- latestRanking(now)
            _       <- F.raiseWhen(ranking.createdAt.atZone(ZoneOffset.UTC).toLocalDate != date)(
              AppError.FailedValidation("The latest completed ranking is not from today; wait for the scheduled ranking run")
            )
            outcome <-
              if ranking.summary.eligibleCount < InvestmentRecipe.MinimumEligibleStocks then
                F.pure[MonthlyScreenOutcome](
                  MonthlyScreenOutcome
                    .InsufficientEligible(ranking.id, ranking.summary.eligibleCount, InvestmentRecipe.MinimumEligibleStocks)
                )
              else
                saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), ranking)
                  .map[MonthlyScreenOutcome](r => MonthlyScreenOutcome.Created(ranking.id, r.summary))
          yield outcome
      _ <- outcome match
        case MonthlyScreenOutcome.Created(rankingId, summary) =>
          logger.info(
            s"Created monthly screen ${summary.id.value}: ranking=${rankingId.value}, eligible=${summary.eligibleCount}, required=${InvestmentRecipe.MinimumEligibleStocks}, selected=${summary.selectedCount}"
          )
        case MonthlyScreenOutcome.AlreadyExists(rankingId, summary) =>
          logger.info(
            s"Monthly screen ${summary.id.value} already exists: ranking=${rankingId.value}, eligible=${summary.eligibleCount}, required=${InvestmentRecipe.MinimumEligibleStocks}"
          )
        case MonthlyScreenOutcome.InsufficientEligible(rankingId, eligible, required) =>
          logger.info(
            s"Monthly screen ${monthlyId.value} skipped: insufficient eligible stocks, ranking=${rankingId.value}, eligible=$eligible, required=$required"
          )
    yield outcome
}

object InvestmentScreenService:
  def make[F[_]: {Sync, Clock, Logger}](
      screens: InvestmentScreenRepository[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentScreenService[F]] =
    Sync[F].pure(LiveInvestmentScreenService(screens, recipe))
