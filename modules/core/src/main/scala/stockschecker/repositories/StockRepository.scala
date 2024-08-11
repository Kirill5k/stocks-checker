package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.Filter
import stockschecker.domain.{Stock, Ticker}
import stockschecker.repositories.entities.StockEntity
import fs2.Stream
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.models.collection.WriteCommand

trait StockRepository[F[_]]:
  def save(stock: Stock): F[Unit]
  def save(stocks: List[Stock]): F[Unit]
  def streamAll: Stream[F, Stock]
  def find(ticker: Ticker): F[Option[Stock]]

final private class LiveStockRepository[F[_]: Concurrent](
    private val collection: MongoCollection[F, StockEntity]
) extends StockRepository[F] {
  
  override def save(stocks: List[Stock]): F[Unit] =
    Stream
      .emits(stocks)
      .map(StockEntity.from)
      .chunkN(1024)
      .mapAsync(4)(chunk => collection.insertMany(chunk.toList))
      .compile
      .drain

  override def save(stock: Stock): F[Unit] =
    collection.insertOne(StockEntity.from(stock)).void

  override def streamAll: Stream[F, Stock] =
    collection.find.stream.map(_.toDomain)

  override def find(ticker: Ticker): F[Option[Stock]] =
    collection.find(Filter.eq("ticker", ticker)).sortByDesc("lastUpdatedAt").first.mapOpt(_.toDomain)
}

object StockRepository:
  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity]("stocks")
      .map(LiveStockRepository[F](_))
