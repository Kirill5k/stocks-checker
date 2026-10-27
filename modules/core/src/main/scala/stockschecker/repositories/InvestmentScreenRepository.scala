package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Index, Sort}
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.repositories.entities.InvestmentScreenEntity

trait InvestmentScreenRepository[F[_]]:
  def save(screen: InvestmentScreen): F[Unit]
  def find(id: InvestmentScreenId): F[InvestmentScreen]
  def get(id: InvestmentScreenId): F[Option[InvestmentScreen]]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def all: F[List[InvestmentScreen]]

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
    collection.find.sort(Sort.desc(Field.CreatedAt)).limit(limit).all.mapList(_.summary)

  override def all: F[List[InvestmentScreen]] =
    collection.find.sort(Sort.asc(Field.CreatedAt)).all.mapList(_.toDomain)
}

object InvestmentScreenRepository:
  val CollectionName = "investment-screens"

  object Field:
    val CreatedAt = "createdAt"
    val RankingId = "rankingId"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentScreenRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[InvestmentScreenEntity](CollectionName)
      _          <- collection.createIndex(Index.descending(Field.CreatedAt))
      _          <- collection.createIndex(Index.ascending(Field.RankingId))
    yield LiveInvestmentScreenRepository[F](collection)
