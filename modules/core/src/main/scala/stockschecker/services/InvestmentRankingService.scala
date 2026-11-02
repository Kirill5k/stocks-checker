package stockschecker.services

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import org.typelevel.log4cats.Logger
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentScorer
import stockschecker.repositories.InvestmentRankingRepository

trait InvestmentRankingService[F[_]]:
  def refresh: F[Unit]

final private class LiveInvestmentRankingService[F[_]](
    rankings: InvestmentRankingRepository[F],
    recipe: InvestmentRecipe
)(using
    F: Sync[F],
    C: Clock[F],
    logger: Logger[F]
) extends InvestmentRankingService[F]:
  override def refresh: F[Unit] =
    for
      candidates <- rankings.findCandidates
      capturedAt <- C.now
      id         <- F.delay(InvestmentRankingId.gen)
      scorecards        = InvestmentScorer.rank(candidates, capturedAt, recipe)
      candidateByTicker = candidates.map(c => c.stock.security.ticker -> c).toMap
      entries           = scorecards.map(card => InvestmentRankingEntry.from(candidateByTicker(card.ticker), card))
      ranking           = InvestmentRanking(id, capturedAt, recipe, InvestmentRankingSummary.from(entries))
      _ <- rankings.save(ranking, entries)
      _ <- logger.info(
        s"Published investment ranking ${id.value}: universe=${ranking.summary.universeCount}, eligible=${ranking.summary.eligibleCount}, createdAt=$capturedAt"
      )
      _ <- rankings.prune(capturedAt.minus(InvestmentRecipe.RankingRetention))
    yield ()

object InvestmentRankingService:
  def make[F[_]: {Sync, Clock, Logger}](
      rankings: InvestmentRankingRepository[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentRankingService[F]] =
    Sync[F].pure(LiveInvestmentRankingService(rankings, recipe))
