package stockschecker.services

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentScorer
import stockschecker.repositories.InvestmentRankingRepository

import java.time.ZoneOffset

trait InvestmentRankingService[F[_]]:
  def refresh: F[Unit]

final private class LiveInvestmentRankingService[F[_]: {Sync, Clock}](
    rankings: InvestmentRankingRepository[F],
    recipe: InvestmentRecipe
) extends InvestmentRankingService[F]:
  override def refresh: F[Unit] =
    for
      now <- Clock[F].now
      date = now.atZone(ZoneOffset.UTC).toLocalDate
      recent <- rankings.latest
      sameDay = recent.filter(r => r.recipe.version == recipe.version && r.createdAt.atZone(ZoneOffset.UTC).toLocalDate == date)
      _ <- sameDay match
        case Some(_) => Sync[F].unit
        case None    =>
          for
            candidates <- rankings.findCandidates
            capturedAt <- Clock[F].now
            id         <- Sync[F].delay(InvestmentRankingId.gen)
            scorecards        = InvestmentScorer.rank(candidates, capturedAt, recipe)
            candidateByTicker = candidates.map(c => c.stock.security.ticker -> c).toMap
            entries           = scorecards.map(card => InvestmentRankingEntry.from(candidateByTicker(card.ticker), card))
            ranking           = InvestmentRanking(id, capturedAt, recipe, InvestmentRankingSummary.from(entries))
            _ <- rankings.save(ranking, entries)
            _ <- rankings.prune(capturedAt.minus(InvestmentRecipe.RankingRetention))
          yield ()
    yield ()

object InvestmentRankingService:
  def make[F[_]: {Sync, Clock}](
      rankings: InvestmentRankingRepository[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentRankingService[F]] =
    Sync[F].pure(LiveInvestmentRankingService(rankings, recipe))
