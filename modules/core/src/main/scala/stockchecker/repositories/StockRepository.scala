package stockchecker.repositories

import cats.Monad
import cats.syntax.functor.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import stockchecker.domain.Stock
import stockchecker.repositories.entities.StockEntity
import fs2.Stream

trait StockRepository[F[_]]:
  def save(stock: Stock): F[Unit]
  def streamAll: Stream[F, Stock]

final private class LiveStockRepository[F[_]: Monad](
    private val collection: MongoCollection[F, StockEntity]
) extends StockRepository[F] {
  
  override def save(stock: Stock): F[Unit] =
    collection.insertOne(StockEntity.from(stock)).void
    
  override def streamAll: Stream[F, Stock] =
    collection.find.stream.map(_.toDomain)
}

object StockRepository:
  def make[F[_]](database: MongoDatabase[F])(using F: Monad[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity]("stocks")
      .map(coll => LiveStockRepository[F](coll))
