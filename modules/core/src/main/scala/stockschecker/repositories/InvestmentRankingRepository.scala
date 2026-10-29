package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort}
import stockschecker.domain.*
import stockschecker.repositories.entities.{InvestmentRankingEntity, InvestmentRankingIdEntity, InvestmentRankingInputEntity}

import java.time.Instant

trait InvestmentRankingRepository[F[_]]:
  def save(ranking: InvestmentRanking, entries: List[InvestmentRankingEntry]): F[Unit]
  def find(id: InvestmentRankingId): F[Option[InvestmentRanking]]
  def findEntries(id: InvestmentRankingId): F[List[InvestmentRankingEntry]]
  def latest: F[Option[InvestmentRanking]]
  def prune(before: Instant): F[Unit]

final private class LiveInvestmentRankingRepository[F[_]: MonadThrow](
    private val collection: MongoCollection[F, InvestmentRankingEntity],
    private val inputs: MongoCollection[F, InvestmentRankingInputEntity]
) extends InvestmentRankingRepository[F] {
  import InvestmentRankingRepository.Field

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

  override def find(id: InvestmentRankingId): F[Option[InvestmentRanking]] =
    collection.find(Filter.idEq(id.value)).first.mapOpt(_.toDomain)

  override def findEntries(id: InvestmentRankingId): F[List[InvestmentRankingEntry]] =
    inputs
      .find(Filter.eq(Field.RankingId, id.value))
      .sort(Sort.asc(Field.Ordinal))
      .all
      .mapList(row => InvestmentRankingEntry(row.input, row.scorecard))

  override def latest: F[Option[InvestmentRanking]] =
    collection.find.sort(Sort.desc(Field.CreatedAt)).first.mapOpt(_.toDomain)

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
        .map(_.map(_._id).toList)
      _ <- MonadThrow[F].whenA(obsolete.nonEmpty)(
        inputs.deleteMany(Filter.in(Field.RankingId, obsolete)).void >>
          collection.deleteMany(Filter.in(Field.Id, obsolete)).void
      )
      published <- collection
        .aggregate[InvestmentRankingIdEntity](Aggregate.project(Projection.include(Field.Id)))
        .all
        .map(_.map(_._id).toList)
      _ <- inputs.deleteMany(Filter.nin(Field.RankingId, published) && Filter.lt(Field.CreatedAt, before))
    yield ()
}

object InvestmentRankingRepository extends MongoJsonCodecs:
  val CollectionName       = "investment-rankings"
  val InputsCollectionName = "investment-ranking-inputs"

  object Field:
    val Id               = "_id"
    val CreatedAt        = "createdAt"
    val RankingId        = "rankingId"
    val Ordinal          = "ordinal"
    val ScreenReferences = "screenReferences"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentRankingRepository[F]] =
    for
      rawCollection <- db.getCollectionWithCodec[InvestmentRankingEntity](CollectionName)
      collection = rawCollection.withAddedCodec[InvestmentRankingIdEntity]
      inputs <- db.getCollectionWithCodec[InvestmentRankingInputEntity](InputsCollectionName)
      _      <- collection.createIndex(Index.descending(Field.CreatedAt))
      _      <- inputs.createIndex(Index.ascending(Field.RankingId))
    yield LiveInvestmentRankingRepository[F](collection, inputs)
