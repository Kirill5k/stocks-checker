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
  def save(header: InvestmentScreenHeader): F[Unit]
  def find(id: InvestmentScreenId): F[InvestmentScreenHeader]
  def header(id: InvestmentScreenId): F[Option[InvestmentScreenHeader]]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def ids: F[List[InvestmentScreenId]]
  def rankingIds: F[Set[InvestmentRankingId]]

final private class LiveInvestmentScreenRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentScreenEntity]
)(using
    F: MonadThrow[F]
) extends InvestmentScreenRepository[F] {

  override def save(header: InvestmentScreenHeader): F[Unit] =
    collection.insertOne(InvestmentScreenEntity.from(header)).void

  override def header(id: InvestmentScreenId): F[Option[InvestmentScreenHeader]] =
    collection.find(Filter.idEq(id.value)).first.mapOpt(_.toDomain)

  override def find(id: InvestmentScreenId): F[InvestmentScreenHeader] =
    header(id).flatMap(_.liftTo[F](AppError.EntityDoesNotExist("Investment screen", id.value)))

  override def list(limit: Int): F[List[InvestmentScreenSummary]] =
    collection.find.sort(Sort.desc("createdAt")).limit(limit).all.mapList(_.summary)

  override def ids: F[List[InvestmentScreenId]] =
    collection.find.sort(Sort.asc("createdAt")).all.mapList(_._id)

  override def rankingIds: F[Set[InvestmentRankingId]] =
    collection
      .aggregate[InvestmentRankingReference](Aggregate.project(Projection.include("rankingId")))
      .all
      .mapList(_.rankingId)
      .map(_.toSet)
}

object InvestmentScreenRepository:
  val CollectionName = "investment-screens"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentScreenRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[InvestmentScreenEntity](CollectionName)
      _          <- collection.createIndex(Index.descending("createdAt"))
    yield LiveInvestmentScreenRepository[F](collection.withAddedCodec[InvestmentRankingReference])
