package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.InvestmentScreenSelection
import stockschecker.repositories.InvestmentScreenRepository

import java.time.{Instant, YearMonth, ZoneOffset}

trait InvestmentScreenService[F[_]]:
  def create(request: InvestmentScreenRequest): F[InvestmentScreenResult]
  def get(id: InvestmentScreenId): F[InvestmentScreenResult]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def createMonthlyScreen: F[Option[InvestmentScreenSummary]]

final private class LiveInvestmentScreenService[F[_]](
    private val screens: InvestmentScreenRepository[F],
    private val recipe: InvestmentRecipe
)(using
    F: Sync[F],
    C: Clock[F]
) extends InvestmentScreenService[F] {
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
      entries <- screens.rankingEntries(ranking.id)
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

  override def createMonthlyScreen: F[Option[InvestmentScreenSummary]] =
    for
      now <- C.now
      date      = now.atZone(ZoneOffset.UTC).toLocalDate
      monthlyId = InvestmentScreenId(s"monthly:${YearMonth.from(date)}:${recipe.version}")
      existing <- screens.get(monthlyId)
      created  <- existing match
        case Some(_) =>
          none[InvestmentScreenSummary].pure[F]
        case None =>
          for
            ranking <- latestRanking(now)
            _       <- F.raiseWhen(ranking.createdAt.atZone(ZoneOffset.UTC).toLocalDate != date)(
              AppError.FailedValidation("The latest completed ranking is not from today; wait for the scheduled ranking run")
            )
            created <-
              if ranking.summary.eligibleCount < InvestmentRecipe.MinimumEligibleStocks then none[InvestmentScreenSummary].pure[F]
              else saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), ranking).map(r => Some(r.summary))
          yield created
    yield created
}

object InvestmentScreenService:
  def make[F[_]: {Sync, Clock}](
      screens: InvestmentScreenRepository[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentScreenService[F]] =
    Sync[F].pure(LiveInvestmentScreenService(screens, recipe))
