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

import java.time.{YearMonth, ZoneOffset}

trait InvestmentScreenService[F[_]]:
  def create(request: InvestmentScreenRequest): F[InvestmentScreenResult]
  def get(id: InvestmentScreenId): F[InvestmentScreenResult]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def generateScheduledScreen: F[Option[InvestmentScreenSummary]]

final private class LiveInvestmentScreenService[F[_]](
    private val screens: InvestmentScreenRepository[F],
    private val rankings: InvestmentRankingService[F],
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
      entries <- rankings.entries(ranking.id)
      now     <- C.now
      selection = InvestmentScreenSelection(entries, request)
      screen    = InvestmentScreen.create(id, now, ranking, request, selection.cohorts)
      _ <- screens.save(screen)
    yield selection.result(screen)

  override def create(request: InvestmentScreenRequest): F[InvestmentScreenResult] =
    for
      _       <- F.fromEither(request.validate(recipe).left.map(AppError.FailedValidation(_)))
      ranking <- rankings.latest
      id      <- F.delay(InvestmentScreenId.gen)
      saved   <- saveScreen(id, request, ranking)
    yield saved

  override def get(id: InvestmentScreenId): F[InvestmentScreenResult] =
    for
      screen    <- screens.find(id)
      published <- rankings.find(screen.rankingId)
      _         <- F.fromOption(published, AppError.EntityDoesNotExist("Investment screen", id.value))
      entries   <- rankings.entries(screen.rankingId)
    yield InvestmentScreenSelection(entries, screen.request).result(screen)

  override def list(limit: Int): F[List[InvestmentScreenSummary]] =
    F.raiseWhen(limit < InvestmentRecipe.MinimumLimit || limit > InvestmentRecipe.MaximumLimit)(
      AppError.FailedValidation("limit must be between 1 and 100")
    ) >> screens.list(limit)

  override def generateScheduledScreen: F[Option[InvestmentScreenSummary]] =
    for
      now <- C.now
      date = now.atZone(ZoneOffset.UTC).toLocalDate
      ranking <- rankings.refresh(date)
      monthlyId = InvestmentScreenId(s"monthly:${YearMonth.from(date)}:${recipe.version}")
      existing <- screens.get(monthlyId)
      created  <- existing match
        case Some(_)                                                                        => none[InvestmentScreenSummary].pure[F]
        case None if ranking.summary.eligibleCount < InvestmentRecipe.MinimumEligibleStocks => none[InvestmentScreenSummary].pure[F]
        case None                                                                           =>
          saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), ranking).map(r => Some(r.summary))
    yield created
}

object InvestmentScreenService:
  def make[F[_]: {Sync, Clock}](
      screens: InvestmentScreenRepository[F],
      rankings: InvestmentRankingService[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentScreenService[F]] =
    Sync[F].pure(LiveInvestmentScreenService(screens, rankings, recipe))
