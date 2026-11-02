package stockschecker.services

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.InvestmentScreenSelection
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.repositories.InvestmentScreenRepository

import java.time.{Instant, YearMonth, ZoneOffset}

trait InvestmentScreenService[F[_]]:
  def rankingIds(ticker: Ticker): F[List[InvestmentRankingId]]
  def delete(ticker: Ticker): F[Unit]
  def create(request: InvestmentScreenRequest): F[InvestmentScreenResult]
  def get(id: InvestmentScreenId, includeExclusions: Boolean = false): F[InvestmentScreenResult]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def createMonthlyScreen: F[MonthlyScreenOutcome]

final private class LiveInvestmentScreenService[F[_]](
    private val screens: InvestmentScreenRepository[F],
    private val dispatcher: ActionDispatcher[F],
    private val recipe: InvestmentRecipe
)(using
    F: Sync[F],
    C: Clock[F],
    logger: Logger[F]
) extends InvestmentScreenService[F] {
  override def rankingIds(ticker: Ticker): F[List[InvestmentRankingId]] = screens.rankingIds(ticker)
  override def delete(ticker: Ticker): F[Unit]                          = screens.delete(ticker)

  private def pin(screen: InvestmentScreen): F[Unit] =
    dispatcher.dispatchOnce(JobId(s"pin-ranking-${screen.id.value}"), Action.PinInvestmentRanking(screen.rankingId))

  private def saveScreen(
      id: InvestmentScreenId,
      request: InvestmentScreenRequest,
      ranking: InvestmentRanking
  ): F[InvestmentScreenResult] =
    for
      entries <- screens.rankingEntries(ranking.id)
      now     <- C.now
      selection = InvestmentScreenSelection(entries, request)
      screen    = InvestmentScreen.create(id, now, ranking, request, selection.cohorts)
      _ <- screens.save(screen)
      _ <- pin(screen)
    yield selection.result(screen)

  override def create(request: InvestmentScreenRequest): F[InvestmentScreenResult] =
    for
      _       <- F.fromEither(request.validate(recipe).left.map(AppError.FailedValidation(_)))
      now     <- C.now
      ranking <- latestRanking(now)
      id      <- F.delay(InvestmentScreenId.gen)
      saved   <- saveScreen(id, request, ranking)
    yield saved

  override def get(id: InvestmentScreenId, includeExclusions: Boolean = false): F[InvestmentScreenResult] =
    for
      context <- screens.findWithRanking(id)
      (screen, _) = context
      entries <- screens.detailEntries(screen, includeExclusions)
      result = InvestmentScreenSelection(entries, screen.request).result(screen)
    yield if includeExclusions then result else result.copy(exclusions = Nil)

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
          pin(screen).as[MonthlyScreenOutcome](MonthlyScreenOutcome.AlreadyExists(screen.rankingId, screen.summary))
        case None =>
          screens.latestRanking.flatMap {
            case None =>
              F.pure[MonthlyScreenOutcome](MonthlyScreenOutcome.RankingUnavailable("No completed ranking is available"))
            case Some(ranking) =>
              val usable = ranking
                .requireUsable(recipe, now)
                .flatMap { value =>
                  Either.cond(
                    value.createdAt.atZone(ZoneOffset.UTC).toLocalDate == date,
                    value,
                    "The latest completed ranking is not from today"
                  )
                }
              usable match
                case Left(reason) => F.pure[MonthlyScreenOutcome](MonthlyScreenOutcome.RankingUnavailable(reason))
                case Right(value) if value.summary.eligibleCount < InvestmentRecipe.MinimumEligibleStocks =>
                  F.pure[MonthlyScreenOutcome](
                    MonthlyScreenOutcome.InsufficientEligible(value.id, value.summary.eligibleCount, InvestmentRecipe.MinimumEligibleStocks)
                  )
                case Right(value) =>
                  saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), value)
                    .map[MonthlyScreenOutcome](r => MonthlyScreenOutcome.Created(value.id, r.summary))
          }
      _ <- outcome match
        case MonthlyScreenOutcome.RankingUnavailable(reason)  => logger.info(s"Monthly screen ${monthlyId.value} skipped: $reason")
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
      dispatcher: ActionDispatcher[F],
      recipe: InvestmentRecipe
  ): F[InvestmentScreenService[F]] =
    Sync[F].pure(LiveInvestmentScreenService(screens, dispatcher, recipe))
