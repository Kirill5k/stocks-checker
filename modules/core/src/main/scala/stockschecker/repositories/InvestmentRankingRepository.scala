package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.*
import stockschecker.repositories.entities.{InvestmentRankingEntity, InvestmentRankingIdEntity}

import java.time.Instant

trait InvestmentRankingRepository[F[_]]:
  def save(header: InvestmentRankingHeader): F[Unit]
  def find(id: InvestmentRankingId): F[Option[InvestmentRankingMetadata]]
  def latest: F[Option[InvestmentRankingMetadata]]
  def updateSummary(id: InvestmentRankingId, summary: InvestmentRankingSummary): F[Unit]
  def ids: F[Set[InvestmentRankingId]]
  def obsolete(before: Instant, referenced: Set[InvestmentRankingId]): F[List[InvestmentRankingId]]
  def delete(ids: List[InvestmentRankingId]): F[Unit]

final private class LiveInvestmentRankingRepository[F[_]: MonadThrow](
    private val collection: MongoCollection[F, InvestmentRankingEntity]
) extends InvestmentRankingRepository[F] {

  override def save(header: InvestmentRankingHeader): F[Unit] =
    collection.insertOne(InvestmentRankingEntity.from(header)).void

  override def find(id: InvestmentRankingId): F[Option[InvestmentRankingMetadata]] =
    collection
      .find(Filter.idEq(id.value))
      .first
      .mapOpt(_.toDomain)

  override def latest: F[Option[InvestmentRankingMetadata]] =
    collection
      .find
      .sort(Sort.desc("createdAt"))
      .first
      .mapOpt(_.toDomain)

  override def updateSummary(id: InvestmentRankingId, summary: InvestmentRankingSummary): F[Unit] =
    collection.updateOne(Filter.idEq(id.value), Update.set("summary", summary)).void

  override def ids: F[Set[InvestmentRankingId]] =
    collection
      .aggregate[InvestmentRankingIdEntity](Aggregate.project(Projection.include("_id")))
      .all
      .mapList(entity => InvestmentRankingId(entity._id))
      .map(_.toSet)

  override def obsolete(before: Instant, referenced: Set[InvestmentRankingId]): F[List[InvestmentRankingId]] =
    collection
      .aggregate[InvestmentRankingIdEntity](
        Aggregate
          .matchBy(Filter.lt("createdAt", before) && Filter.nin("_id", referenced.toList.map(_.value)))
          .project(Projection.include("_id"))
      )
      .all
      .mapList(entity => InvestmentRankingId(entity._id))

  override def delete(ids: List[InvestmentRankingId]): F[Unit] =
    if ids.isEmpty then ().pure[F]
    else collection.deleteMany(Filter.in("_id", ids.map(_.value))).void
}

object InvestmentRankingRepository extends MongoJsonCodecs:
  val CollectionName = "investment-rankings"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentRankingRepository[F]] =
    for
      rawCollection <- db.getCollectionWithCodec[InvestmentRankingEntity](CollectionName)
      collection = rawCollection.withAddedCodec[InvestmentRankingIdEntity].withAddedCodec[InvestmentRankingSummary]
      _ <- collection.createIndex(Index.descending("createdAt"))
    yield LiveInvestmentRankingRepository[F](collection)
