package stockschecker.services

import stockschecker.utils.InvestmentScreenSelection

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.InvestmentScreenRepository
import java.time.ZoneOffset

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
  private def validate(request: InvestmentScreenRequest): F[Unit] =
    val thresholds = List(request.minScore, request.minQuality, request.minValue, request.minGrowth, request.minResilience).flatten
    Sync[F].raiseWhen(request.limit.exists(n => n < 1 || n > 100) || thresholds.exists(n => n < 0 || n > 100))(
      AppError.FailedValidation("limit must be between 1 and 100; score thresholds must be between 0 and 100")
    ) >>
      Sync[F].raiseWhen(request.recipeVersion.exists(_ != recipe.version))(
        AppError.FailedValidation(s"Unknown recipeVersion; supported version: ${recipe.version}")
      )

  private def saveScreen(
      id: InvestmentScreenId,
      request: InvestmentScreenRequest,
      ranking: InvestmentRankingRun
  ): F[InvestmentScreenResult] =
    for
      now <- Clock[F].now
      selection = InvestmentScreenSelection(ranking.inputs, ranking.scorecards, request)
      run       = InvestmentScreenRun(
        id,
        now,
        ranking.recipe,
        request,
        ranking.inputs,
        ranking.scorecards,
        selection.cohorts,
        ranking.id
      )
      _ <- screens.save(InvestmentScreenHeader.from(run))
    yield selection.result(run)

  override def create(request: InvestmentScreenRequest): F[InvestmentScreenResult] =
    for
      _       <- validate(request)
      ranking <- rankings.latest
      id      <- Sync[F].delay(InvestmentScreenId.gen)
      saved   <- saveScreen(id, request, ranking)
    yield saved

  override def get(id: InvestmentScreenId): F[InvestmentScreenResult] =
    for
      screen    <- screens.find(id)
      published <- rankings.find(screen.rankingId)
      ranking   <- Sync[F].fromOption(published, AppError.EntityDoesNotExist("Investment screen", id.value))
    yield InvestmentScreenSelection(ranking.inputs, ranking.scorecards, screen.request).result(
      InvestmentScreenRun(
        screen.id,
        screen.createdAt,
        ranking.recipe,
        screen.request,
        ranking.inputs,
        ranking.scorecards,
        screen.cohorts,
        ranking.id
      )
    )
  override def list(limit: Int): F[List[InvestmentScreenSummary]] =
    Sync[F].raiseWhen(limit < 1 || limit > 100)(AppError.FailedValidation("limit must be between 1 and 100")) >> screens.list(limit)

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
          rankings
            .get(header.id)
            .flatMap { ranking =>
              saveScreen(monthlyId, InvestmentScreenRequest(recipeVersion = Some(recipe.version)), ranking).map(r => Some(r.summary))
            }
    yield created

}

object InvestmentScreenService:
  def make[F[_]: {Sync, Clock}](
      screens: InvestmentScreenRepository[F],
      rankings: InvestmentRankingService[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentScreenService[F]] =
    Sync[F].pure(LiveInvestmentScreenService(screens, rankings, recipe))
