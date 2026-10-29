package stockschecker.repositories

import cats.{Applicative, MonadThrow}
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Projection, Sort}
import stockschecker.domain.*
import stockschecker.repositories.entities.{
  InvestmentCandidateEntity,
  InvestmentRankingEntity,
  InvestmentRankingIdEntity,
  InvestmentRankingInputEntity
}

import java.time.Instant

trait InvestmentRankingRepository[F[_]]:
  def findCandidates: F[List[InvestmentCandidate]]
  def save(ranking: InvestmentRanking, entries: List[InvestmentRankingEntry]): F[Unit]
  def latest: F[Option[InvestmentRanking]]
  def prune(before: Instant): F[Unit]

final private class LiveInvestmentRankingRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentRankingEntity],
    private val inputs: MongoCollection[F, InvestmentRankingInputEntity],
    private val securities: MongoCollection[F, InvestmentCandidateEntity]
)(using
    F: MonadThrow[F]
) extends InvestmentRankingRepository[F] {
  import InvestmentRankingRepository.Field

  override def findCandidates: F[List[InvestmentCandidate]] =
    val stock = StockAggregates.Field
    securities
      .aggregate[InvestmentCandidateEntity](
        StockAggregates
          .build(Filter.empty, StockFilters(), None)
          .replaceWith(Document(Field.Stock := "$$ROOT"))
          .lookup(
            FinancialHistoryRepository.CollectionName,
            s"${Field.Stock}.${stock.Security}.${stock.Id}",
            stock.Id,
            Field.FinancialSnapshots
          )
          .sort(Sort.asc(s"${Field.Stock}.${stock.Security}.${stock.Id}"))
      )
      .all
      .mapList(_.toDomain)

  override def save(ranking: InvestmentRanking, entries: List[InvestmentRankingEntry]): F[Unit] =
    val rows = entries.zipWithIndex.map { case (entry, ordinal) =>
      InvestmentRankingInputEntity(
        s"${ranking.id.value}:${entry.scorecard.ticker.value}",
        ranking.id,
        ordinal,
        entry.input,
        entry.scorecard,
        ranking.createdAt
      )
    }
    // Publish metadata only after every input batch is durable. The universe can exceed MongoDB's document size limit.
    rows.grouped(100).toList.traverse_(batch => inputs.insertMany(batch).void) >>
      collection.insertOne(InvestmentRankingEntity.from(ranking)).void

  override def latest: F[Option[InvestmentRanking]] =
    InvestmentRankingQueries.latest(collection)

  override def prune(before: Instant): F[Unit] =
    for
      obsolete <- collection
        .aggregate[InvestmentRankingIdEntity](
          Aggregate
            .matchBy(Filter.lt(Field.CreatedAt, before))
            .lookup(InvestmentScreenRepository.CollectionName, Field.Id, InvestmentScreenRepository.Field.RankingId, Field.ScreenReferences)
            .matchBy(Filter.size(Field.ScreenReferences, 0))
            .project(Projection.include(Field.Id))
        )
        .all
        .mapList(_._id)
      _ <- F.whenA(obsolete.nonEmpty)(
        inputs.deleteMany(Filter.in(Field.RankingId, obsolete)).void >>
          collection.deleteMany(Filter.in(Field.Id, obsolete)).void
      )
      orphaned <- inputs
        .aggregate[InvestmentRankingIdEntity](
          Aggregate
            .matchBy(Filter.lt(Field.CreatedAt, before))
            .group(s"$$${Field.RankingId}", Accumulator)
            .lookup(InvestmentRankingRepository.CollectionName, Field.Id, Field.Id, Field.PublishedRankings)
            .matchBy(Filter.size(Field.PublishedRankings, 0))
            .project(Projection.include(Field.Id))
        )
        .all
        .mapList(_._id)
      _ <- F.whenA(orphaned.nonEmpty)(
        inputs.deleteMany(Filter.in(Field.RankingId, orphaned) && Filter.lt(Field.CreatedAt, before)).void
      )
    yield ()
}

object InvestmentRankingRepository extends MongoJsonCodecs:
  val CollectionName       = "investment-rankings"
  val InputsCollectionName = "investment-ranking-inputs"

  object Field:
    val Id                 = "_id"
    val CreatedAt          = "createdAt"
    val RankingId          = "rankingId"
    val Ordinal            = "ordinal"
    val ScreenReferences   = "screenReferences"
    val PublishedRankings  = "publishedRankings"
    val Stock              = "stock"
    val FinancialSnapshots = "financialSnapshots"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentRankingRepository[F]] =
    for
      rawCollection <- db.getCollectionWithCodec[InvestmentRankingEntity](CollectionName)
      collection = rawCollection.withAddedCodec[InvestmentRankingIdEntity]
      rawInputs <- db.getCollectionWithCodec[InvestmentRankingInputEntity](InputsCollectionName)
      inputs = rawInputs.withAddedCodec[InvestmentRankingIdEntity]
      securities <- db.getCollectionWithCodec[InvestmentCandidateEntity](SecurityRepository.CollectionName)
      _          <- collection.createIndex(Index.descending(Field.CreatedAt))
      _          <- inputs.createIndex(Index.ascending(Field.RankingId))
    yield LiveInvestmentRankingRepository[F](collection, inputs, securities)

private[repositories] object InvestmentRankingQueries:
  def latest[F[_]: Applicative](collection: MongoCollection[F, InvestmentRankingEntity]): F[Option[InvestmentRanking]] =
    collection.find.sort(Sort.desc(InvestmentRankingRepository.Field.CreatedAt)).first.mapOpt(_.toDomain)
