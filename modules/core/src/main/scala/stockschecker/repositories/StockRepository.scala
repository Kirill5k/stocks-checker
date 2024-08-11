package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.{Stock, Ticker}
import stockschecker.repositories.entities.StockEntity
import fs2.Stream
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}

trait StockRepository[F[_]]:
  def save(stock: Stock): F[Unit]
  def save(stocks: List[Stock]): F[Unit]
  def streamAll: Stream[F, Stock]
  def find(ticker: Ticker): F[Option[Stock]]

final private class LiveStockRepository[F[_]: Concurrent](
    private val collection: MongoCollection[F, StockEntity]
) extends StockRepository[F] {
  
  extension (stock: Stock)
    def id: String = s"${stock.ticker}.${stock.lastUpdatedAt.toString.substring(0, 10)}"
    def toUpdate: Update =
      Update
        .setOnInsert("_id", id)
        .setOnInsert("ticker", stock.ticker)
        .setOnInsert("name", stock.name)
        .setOnInsert("exchange", stock.exchange)
        .setOnInsert("exchangeShortName", stock.exchangeShortName)
        .setOnInsert("stockType", stock.stockType)
        .set("price", stock.price)
        .set("lastUpdatedAt", stock.lastUpdatedAt)

  override def save(stocks: List[Stock]): F[Unit] =
    Stream
      .emits(stocks)
      .map(s => WriteCommand.UpdateOne(Filter.idEq(s.id), s.toUpdate, UpdateOptions(upsert = true)))
      .chunkN(1024)
      .mapAsync(4)(chunk => collection.bulkWrite(chunk.toList))
      .compile
      .drain

  override def save(stock: Stock): F[Unit] =
    collection.updateOne(Filter.idEq(stock.id), stock.toUpdate, UpdateOptions(upsert = true)).void

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
