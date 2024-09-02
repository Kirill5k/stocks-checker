package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import com.mongodb.client.model.UnwindOptions
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Projection, Sort, Update}
import stockschecker.domain.{Stock, Ticker}
import stockschecker.repositories.entities.StockEntity
import fs2.Stream
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.{BsonValue, Document}
import mongo4cats.bson.syntax.*
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}

trait StockRepository[F[_]]:
  def save(stock: Stock): F[Unit]
  def save(stocks: List[Stock]): F[Unit]
  def streamAll: Stream[F, Stock]
  def find(ticker: Ticker, limit: Option[Int]): F[List[Stock]]
  def withPriceDeltas(ticker: Ticker, limit: Option[Int]): F[List[Stock]]

final private class LiveStockRepository[F[_]: Concurrent](
    private val collection: MongoCollection[F, StockEntity]
) extends StockRepository[F] {

  private object Field:
    val Id            = "_id"
    val StockType     = "stockType"
    val Ticker        = "ticker"
    val LastUpdatedAt = "lastUpdatedAt"
    val Price         = "price"
    val PriceDelta    = "priceDelta"

  extension (stock: Stock)
    private def id: String = s"${stock.ticker}.${stock.lastUpdatedAt.toString.substring(0, 10)}"
    private def toUpdateCommand: WriteCommand[Nothing] =
      val id = stock.id
      WriteCommand.UpdateOne(
        Filter.idEq(id),
        Update
          .setOnInsert(Field.Id, id)
          .setOnInsert(Field.Ticker, stock.ticker)
          .setOnInsert(Field.StockType, stock.stockType)
          .set(Field.Price, stock.price)
          .set(Field.LastUpdatedAt, stock.lastUpdatedAt),
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
      .find(Filter.eq(Field.Ticker, ticker))
      .sortByDesc(Field.LastUpdatedAt)
      .limit(limit.getOrElse(Int.MaxValue))
      .all
      .mapList(_.toDomain)

  override def withPriceDeltas(ticker: Ticker, limit: Option[Int]): F[List[Stock]] =
    collection
      .aggregate[Stock](
        Aggregate
          .sort(Sort.asc(Field.Ticker).asc(Field.LastUpdatedAt))
          .group(
            "$" + Field.Ticker,
            Accumulator
              .first(Field.StockType, "$" + Field.StockType)
              .push("prices", Document(Field.Price := "$" + Field.Price, Field.LastUpdatedAt := "$" + Field.LastUpdatedAt))
          )
          .unwind(
            "$prices",
            new UnwindOptions().includeArrayIndex("$" + Field.LastUpdatedAt)
          )
          .set(Field.PriceDelta -> Document("$subtract" := List(
            BsonValue.string("$prices.price"),
            BsonValue.document("$arrayElemAt" := List(
              BsonValue.string("$prices.price"),
              BsonValue.document("$subtract" := List(BsonValue.string("$index"), BsonValue.int(1)))
            ))
          )))
          .set(Field.PriceDelta -> Document("$cond" := BsonValue.document(
            "if" := BsonValue.document("$eq" := BsonValue.array(BsonValue.string("$index"), BsonValue.int(0))),
            "then" -> BsonValue.BNull,
            "else" := "$" + Field.PriceDelta
          )))
          .project(
            Projection
              .computed(Field.Ticker, "$" + "_id")
              .computed(Field.Price, "$" + "prices.price")
              .computed(Field.LastUpdatedAt, "$" + "prices.lastUpdatedDate")
              .include(Field.StockType)
              .include(Field.PriceDelta)
              .excludeId
          )
      )
      .all.map(_.toList)
}

object StockRepository:
  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity]("stocks")
      .map(LiveStockRepository[F](_))
