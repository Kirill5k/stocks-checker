package stockschecker.services

import java.time.Instant

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.domain.*
import stockschecker.domain.investment.InvestmentScorer
import stockschecker.repositories.InvestmentRankingRepository

trait InvestmentRankingService[F[_]]:
  def delete(ticker: Ticker): F[Unit]
  def pin(id: InvestmentRankingId): F[Unit]
  def release(id: InvestmentRankingId, requestedAt: Instant): F[Unit]
  def refresh: F[Unit]
  def rankingInputs(id: InvestmentRankingId, query: RankingInputsQuery): F[RankingInputsPage]

final private class LiveInvestmentRankingService[F[_]](
    rankings: InvestmentRankingRepository[F],
    recipe: InvestmentRecipe
)(using
    F: Sync[F],
    C: Clock[F],
    logger: Logger[F]
) extends InvestmentRankingService[F]:
  override def delete(ticker: Ticker): F[Unit]                                 = rankings.delete(ticker)
  override def pin(id: InvestmentRankingId): F[Unit]                           = rankings.pin(id)
  override def release(id: InvestmentRankingId, requestedAt: Instant): F[Unit] = rankings.release(id, requestedAt)

  override def rankingInputs(id: InvestmentRankingId, query: RankingInputsQuery): F[RankingInputsPage] =
    rankings.rankingInputs(id, query)

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
    yield ()

object InvestmentRankingService:
  def make[F[_]: {Sync, Clock, Logger}](
      rankings: InvestmentRankingRepository[F],
      recipe: InvestmentRecipe
  ): F[InvestmentRankingService[F]] =
    Sync[F].pure(LiveInvestmentRankingService(rankings, recipe))
