package stockschecker.services

import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{
  FinancialHistoryRepository,
  InvestmentRankingInputRepository,
  InvestmentRankingRepository,
  InvestmentScreenRepository,
  StockRepository
}

import java.time.{Instant, LocalDate, ZoneOffset}

trait InvestmentRankingService[F[_]]:
  def latest: F[InvestmentRankingRun]
  def latestHeader: F[Option[InvestmentRankingHeader]]
  def refresh(date: LocalDate): F[InvestmentRankingHeader]
  def find(id: InvestmentRankingId): F[Option[InvestmentRankingRun]]
  def get(id: InvestmentRankingId): F[InvestmentRankingRun]
  def prune(before: Instant): F[Unit]

final private class LiveInvestmentRankingService[F[_]: {Sync, Clock}](
    stockRepository: StockRepository[F],
    financialHistory: FinancialHistoryRepository[F],
    rankings: InvestmentRankingRepository[F],
    inputs: InvestmentRankingInputRepository[F],
    screens: InvestmentScreenRepository[F],
    recipe: InvestmentRecipe
) extends InvestmentRankingService[F]:
  private def resolve(metadata: InvestmentRankingMetadata): F[InvestmentRankingHeader] =
    metadata.summary match
      case Some(summary) => metadata.resolved(summary).pure[F]
      case None          =>
        inputs.summary(metadata.id).flatMap { summary =>
          rankings.updateSummary(metadata.id, summary).as(metadata.resolved(summary))
        }

  private def hydrate(header: InvestmentRankingHeader): F[InvestmentRankingRun] =
    inputs
      .find(header.id)
      .map { rows =>
        InvestmentRankingRun(header.id, header.createdAt, header.recipe, rows.inputs, rows.scorecards)
      }

  override def latestHeader: F[Option[InvestmentRankingHeader]] = rankings.latest.flatMap(_.traverse(resolve))

  override def latest: F[InvestmentRankingRun] =
    for
      published <- latestHeader
      header    <- Sync[F].fromOption(
        published.filter(_.recipe.version == recipe.version),
        AppError.FailedValidation("No completed ranking for this recipe is available; wait for the scheduled ranking run")
      )
      now <- Clock[F].now
      _   <- Sync[F].raiseWhen(header.createdAt.isBefore(now.minusSeconds(2 * 86400)))(
        AppError.FailedValidation("The latest completed ranking is stale")
      )
      ranking <- hydrate(header)
    yield ranking

  override def refresh(date: LocalDate): F[InvestmentRankingHeader] =
    for
      recent <- latestHeader
      sameDay = recent.filter(r => r.recipe.version == recipe.version && r.createdAt.atZone(ZoneOffset.UTC).toLocalDate == date)
      header <- sameDay match
        case Some(value) => value.pure[F]
        case None        =>
          for
            stocks     <- stockRepository.findAll(StockFilters(), None)
            snapshots  <- financialHistory.findLatestAll
            capturedAt <- Clock[F].now
            id         <- Sync[F].delay(InvestmentRankingId.gen)
            byTicker   = snapshots.map(s => s.ticker -> s).toMap
            candidates = stocks.sortBy(_.security.ticker.value).map(s => InvestmentCandidate(s, byTicker.get(s.security.ticker)))
            scorecards = InvestmentScorer.rank(candidates, capturedAt, recipe)
            run        = InvestmentRankingRun(id, capturedAt, recipe, candidates, scorecards)
            header     = InvestmentRankingHeader.from(run)
            // The header publishes the ranking only after every input has been saved.
            _ <- inputs.save(run)
            _ <- rankings.save(header)
            _ <- prune(capturedAt.minusSeconds(30L * 86400))
          yield header
    yield header

  override def find(id: InvestmentRankingId): F[Option[InvestmentRankingRun]] =
    rankings.find(id).flatMap(_.traverse(metadata => resolve(metadata).flatMap(hydrate)))

  override def get(id: InvestmentRankingId): F[InvestmentRankingRun] =
    find(id).flatMap(Sync[F].fromOption(_, new IllegalStateException("Published investment ranking disappeared")))

  override def prune(before: Instant): F[Unit] =
    for
      referenced <- screens.rankingIds
      obsolete   <- rankings.obsolete(before, referenced)
      _          <- inputs.delete(obsolete)
      _          <- rankings.delete(obsolete)
      published  <- rankings.ids
      _          <- inputs.pruneOrphans(published, before)
    yield ()

object InvestmentRankingService:
  def make[F[_]: {Sync, Clock}](
      stockRepository: StockRepository[F],
      financialHistory: FinancialHistoryRepository[F],
      rankings: InvestmentRankingRepository[F],
      inputs: InvestmentRankingInputRepository[F],
      screens: InvestmentScreenRepository[F],
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): F[InvestmentRankingService[F]] =
    Sync[F].pure(LiveInvestmentRankingService(stockRepository, financialHistory, rankings, inputs, screens, recipe))
