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
  def find(ticker: Ticker, limit: Option[Int]): F[List[Stock]]

final private class LiveStockRepository[F[_]: Concurrent](
    private val collection: MongoCollection[F, StockEntity]
) extends StockRepository[F] {

  extension (stock: Stock)
    private def id: String = s"${stock.ticker}.${stock.lastUpdatedAt.toString.substring(0, 10)}"
    private def toUpdateCommand: WriteCommand[Nothing] =
      val id = stock.id
      WriteCommand.UpdateOne(
        Filter.idEq(id),
        Update
          .setOnInsert("_id", id)
          .setOnInsert("ticker", stock.ticker)
          .setOnInsert("stockType", stock.stockType)
          .set("price", stock.price)
          .set("lastUpdatedAt", stock.lastUpdatedAt),
        UpdateOptions(upsert = true)
      )

  override def save(stocks: List[Stock]): F[Unit] =
    collection.bulkWrite(stocks.map(_.toUpdateCommand)).void

  override def save(stock: Stock): F[Unit] =
    collection.bulkWrite(List(stock.toUpdateCommand)).void

  override def streamAll: Stream[F, Stock] =
    collection.find.stream.map(_.toDomain)

  override def find(ticker: Ticker, limit: Option[Int]): F[List[Stock]] =
    collection
      .find(Filter.eq("ticker", ticker))
      .sortByDesc("lastUpdatedAt")
      .limit(limit.getOrElse(Int.MaxValue))
      .all
      .mapList(_.toDomain)
}

object StockRepository:
  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity]("stocks")
      .map(LiveStockRepository[F](_))
