package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.domain.investment.InvestmentScorer
import stockschecker.repositories.InvestmentRankingRepository

import java.time.{Instant, LocalDate, ZoneOffset}

trait InvestmentRankingService[F[_]]:
  def latest: F[InvestmentRanking]
  def latestAvailable: F[Option[InvestmentRanking]]
  def refresh(date: LocalDate): F[InvestmentRanking]
  def find(id: InvestmentRankingId): F[Option[InvestmentRanking]]
  def entries(id: InvestmentRankingId): F[List[InvestmentRankingEntry]]
  def prune(before: Instant): F[Unit]

final private class LiveInvestmentRankingService[F[_]: {Sync, Clock}](
    stocks: StockService[F],
    rankings: InvestmentRankingRepository[F],
    recipe: InvestmentRecipe
) extends InvestmentRankingService[F]:
  override def latestAvailable: F[Option[InvestmentRanking]] = rankings.latest

  override def latest: F[InvestmentRanking] =
    for
      published <- latestAvailable
      ranking   <- Sync[F].fromOption(
        published.filter(_.recipe.version == recipe.version),
        AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run")
      )
      now <- Clock[F].now
      _   <- Sync[F].raiseWhen(ranking.createdAt.isBefore(now.minus(InvestmentRecipe.RankingMaxAge)))(
        AppError.FailedValidation("The latest completed ranking is stale")
      )
    yield ranking

  override def refresh(date: LocalDate): F[InvestmentRanking] =
    for
      recent <- latestAvailable
      sameDay = recent.filter(r => r.recipe.version == recipe.version && r.createdAt.atZone(ZoneOffset.UTC).toLocalDate == date)
      ranking <- sameDay match
        case Some(value) => value.pure[F]
        case None        =>
          for
            candidates <- stocks.findInvestmentCandidates
            capturedAt <- Clock[F].now
            id         <- Sync[F].delay(InvestmentRankingId.gen)
            scorecards        = InvestmentScorer.rank(candidates, capturedAt, recipe)
            candidateByTicker = candidates.map(c => c.stock.security.ticker -> c).toMap
            entries           = scorecards.map(card => InvestmentRankingEntry(candidateByTicker(card.ticker).compact, card))
            ranking           = InvestmentRanking(id, capturedAt, recipe, InvestmentRankingSummary.from(entries))
            _ <- rankings.save(ranking, entries)
            _ <- prune(capturedAt.minus(InvestmentRecipe.RankingRetention))
          yield ranking
    yield ranking

  override def find(id: InvestmentRankingId): F[Option[InvestmentRanking]] = rankings.find(id)

  override def entries(id: InvestmentRankingId): F[List[InvestmentRankingEntry]] = rankings.findEntries(id)

  override def prune(before: Instant): F[Unit] = rankings.prune(before)

object InvestmentRankingService:
  def make[F[_]: {Sync, Clock}](
      stocks: StockService[F],
      rankings: InvestmentRankingRepository[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentRankingService[F]] =
    Sync[F].pure(LiveInvestmentRankingService(stocks, rankings, recipe))
