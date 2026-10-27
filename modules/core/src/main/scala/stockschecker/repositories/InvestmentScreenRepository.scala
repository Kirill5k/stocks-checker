package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.entities.{InvestmentRankingReference, InvestmentScreenEntity}

trait InvestmentScreenRepository[F[_]]:
  def save(screen: InvestmentScreen): F[Unit]
  def find(id: InvestmentScreenId): F[InvestmentScreen]
  def get(id: InvestmentScreenId): F[Option[InvestmentScreen]]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def ids: F[List[InvestmentScreenId]]
  def rankingIds: F[Set[InvestmentRankingId]]

final private class LiveInvestmentScreenRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentScreenEntity]
)(using
    F: MonadThrow[F]
) extends InvestmentScreenRepository[F] {
  import InvestmentScreenRepository.Field

  override def save(screen: InvestmentScreen): F[Unit] =
    collection.insertOne(InvestmentScreenEntity.from(screen)).void

  override def get(id: InvestmentScreenId): F[Option[InvestmentScreen]] =
    collection.find(Filter.idEq(id.value)).first.mapOpt(_.toDomain)

  override def find(id: InvestmentScreenId): F[InvestmentScreen] =
    get(id).flatMap(_.liftTo[F](AppError.EntityDoesNotExist("Investment screen", id.value)))

  override def list(limit: Int): F[List[InvestmentScreenSummary]] =
    collection.find.sort(Sort.desc(Field.createdAt)).limit(limit).all.mapList(_.summary)

  override def ids: F[List[InvestmentScreenId]] =
    collection.find.sort(Sort.asc(Field.createdAt)).all.mapList(_._id)

  override def rankingIds: F[Set[InvestmentRankingId]] =
    collection
      .aggregate[InvestmentRankingReference](Aggregate.project(Projection.include(Field.rankingId)))
      .all
      .mapList(_.rankingId)
      .map(_.toSet)
}

object InvestmentScreenRepository:
  val CollectionName = "investment-screens"

  object Field:
    val createdAt = "createdAt"
    val rankingId = "rankingId"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentScreenRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[InvestmentScreenEntity](CollectionName)
      _          <- collection.createIndex(Index.descending(Field.createdAt))
    yield LiveInvestmentScreenRepository[F](collection.withAddedCodec[InvestmentRankingReference])
