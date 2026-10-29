package stockschecker.repositories

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.functor.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Projection}
import stockschecker.domain.{Exchange, SecurityKind, Stock, StockFilters, Ticker}
import stockschecker.repositories.entities.{Entity, StockEntity}

trait StockRepository[F[_]]:
  def exists(ticker: Ticker): F[Boolean]
  def find(ticker: Ticker): F[Option[Stock]]
  def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]]

final private class LiveStockRepository[F[_]: Monad](
    private val collection: MongoCollection[F, StockEntity]
) extends StockRepository[F]:
  import StockAggregates.*

  override def exists(ticker: Ticker): F[Boolean] =
    collection
      .aggregate[Entity](Aggregate.matchBy(Filter.idEq(ticker.value)).project(Projection.include(SecurityRepository.Field.Id)).limit(1))
      .first
      .map(_.isDefined)

  override def find(ticker: Ticker): F[Option[Stock]] =
    collection
      .aggregate[StockEntity](build(Filter.idEq(ticker.value), StockFilters(), Some(1)))
      .first
      .mapOpt(_.toDomain)

  override def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](build(filters.toSecurityFilter, filters, Some(limit.getOrElse(Int.MaxValue))))
      .all
      .mapList(_.toDomain)

object StockRepository extends MongoJsonCodecs:
  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity](SecurityRepository.CollectionName)
      .map(_.withAddedCodec[Exchange].withAddedCodec[SecurityKind].withAddedCodec[Entity])
      .map(LiveStockRepository[F](_))
