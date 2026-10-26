package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{FinancialHistoryRepository, InvestmentRankingRepository, StockRepository}

import java.time.{LocalDate, ZoneOffset}
import java.util.UUID

trait InvestmentRankingService[F[_]]:
  def latest: F[InvestmentRankingRun]
  def refresh(date: LocalDate): F[InvestmentRankingRepository.Header]
  def get(id: String): F[InvestmentRankingRun]

final private class LiveInvestmentRankingService[F[_]: {Sync, Clock}](
    stockRepository: StockRepository[F],
    financialHistory: FinancialHistoryRepository[F],
    rankings: InvestmentRankingRepository[F],
    recipe: InvestmentRecipe
) extends InvestmentRankingService[F]:
  override def latest: F[InvestmentRankingRun] =
    for
      published <- rankings.latest
      ranking   <- Sync[F].fromOption(
        published.filter(_.recipe.version == recipe.version),
        AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run")
      )
      now <- Clock[F].now
      _   <- Sync[F].raiseWhen(ranking.createdAt.isBefore(now.minusSeconds(2 * 86400)))(
        AppError.FailedValidation("The latest completed ranking is stale")
      )
    yield ranking

  override def refresh(date: LocalDate): F[InvestmentRankingRepository.Header] =
    for
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
    yield header

  override def get(id: String): F[InvestmentRankingRun] =
    rankings.find(id).flatMap(Sync[F].fromOption(_, new IllegalStateException("Published investment ranking disappeared")))

object InvestmentRankingService:
  def make[F[_]: {Sync, Clock}](
      stockRepository: StockRepository[F],
      financialHistory: FinancialHistoryRepository[F],
      rankings: InvestmentRankingRepository[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentRankingService[F]] =
    Sync[F].pure(LiveInvestmentRankingService(stockRepository, financialHistory, rankings, recipe))
