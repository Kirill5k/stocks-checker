package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort}
import stockschecker.domain.*
import stockschecker.repositories.entities.{InvestmentRankingInputEntity, InvestmentRankingOutcome}

import java.time.Instant

trait InvestmentRankingInputRepository[F[_]]:
  def save(run: InvestmentRankingRun): F[Unit]
  def find(id: InvestmentRankingId): F[InvestmentRankingInputs]
  def summary(id: InvestmentRankingId): F[InvestmentRankingSummary]
  def delete(ids: List[InvestmentRankingId]): F[Unit]
  def pruneOrphans(published: Set[InvestmentRankingId], before: Instant): F[Unit]

final private class LiveInvestmentRankingInputRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentRankingInputEntity]
)(using
    F: MonadThrow[F]
) extends InvestmentRankingInputRepository[F] {

  private def compact(candidate: InvestmentCandidate): InvestmentCandidate =
    candidate.copy(financialSnapshot =
      candidate.financialSnapshot.map(snapshot =>
        snapshot.copy(
          metrics = snapshot.metrics.filter { case (metric, _) => Set("peTTM", "pfcfShareTTM", "epsTTM")(metric) },
          annual = Nil,
          quarterly = Nil,
          rawPayload = ""
        )
      )
    )

  override def save(run: InvestmentRankingRun): F[Unit] =
    val candidates = run.inputs.zipWithIndex.map { case (candidate, ordinal) =>
      candidate.stock.security.ticker -> (candidate, ordinal)
    }.toMap
    val rows = run.scorecards.zipWithIndex.map { case (scorecard, ordinal) =>
      val (candidate, inputOrdinal) = candidates(scorecard.ticker)
      InvestmentRankingInputEntity(
        s"${run.id.value}:${scorecard.ticker.value}",
        run.id,
        ordinal,
        inputOrdinal,
        compact(candidate),
        scorecard,
        Some(run.createdAt)
      )
    }
    // Store candidates separately because the complete-market universe may exceed MongoDB's document size limit.
    rows.grouped(100).toList.traverse_(batch => collection.insertMany(batch).void)

  override def find(id: InvestmentRankingId): F[InvestmentRankingInputs] =
    collection
      .find(Filter.eq("rankingId", id.value))
      .sort(Sort.asc("ordinal"))
      .all
      .map { rows =>
        InvestmentRankingInputs(rows.toList.sortBy(_.inputOrdinal).map(_.candidate), rows.map(_.scorecard).toList)
      }

  override def summary(id: InvestmentRankingId): F[InvestmentRankingSummary] =
    collection
      .aggregate[InvestmentRankingOutcome](
        Aggregate
          .matchBy(Filter.eq("rankingId", id.value))
          .project(
            Projection.exclude("_id").computed("eligible", "$scorecard.eligible").computed("exclusions", "$scorecard.exclusions")
          )
      )
      .all
      .map { outcomes =>
        InvestmentRankingSummary(
          outcomes.size,
          outcomes.count(_.eligible),
          outcomes.toList.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _)
        )
      }

  override def delete(ids: List[InvestmentRankingId]): F[Unit] =
    if ids.isEmpty then F.unit
    else collection.deleteMany(Filter.in("rankingId", ids.map(_.value))).void

  override def pruneOrphans(published: Set[InvestmentRankingId], before: Instant): F[Unit] =
    collection
      .deleteMany(
        Filter.nin("rankingId", published.toList.map(_.value)) &&
          (Filter.lt("createdAt", before) || Filter.isNull("createdAt"))
      )
      .void
}

object InvestmentRankingInputRepository extends MongoJsonCodecs:
  val CollectionName = "investment-ranking-inputs"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentRankingInputRepository[F]] =
    for
      rawCollection <- db.getCollectionWithCodec[InvestmentRankingInputEntity](CollectionName)
      collection = rawCollection.withAddedCodec[InvestmentRankingOutcome]
      _ <- collection.createIndex(Index.ascending("rankingId"))
    yield LiveInvestmentRankingInputRepository[F](collection)
