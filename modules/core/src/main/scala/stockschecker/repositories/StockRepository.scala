package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import com.mongodb.client.model.WindowOutputFields
import fs2.Stream
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.syntax.*
import mongo4cats.bson.{BsonValue, Document}
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Aggregate, Filter, Sort, Update}
import stockschecker.domain.{Stock, Ticker}
import stockschecker.repositories.entities.StockEntity

trait StockRepository[F[_]]:
  def save(stock: Stock): F[Unit]
  def save(stocks: List[Stock]): F[Unit]
  def streamAll: Stream[F, Stock]
  def find(ticker: Ticker, limit: Option[Int]): F[List[Stock]]
  def findWithPriceDeltas(ticker: Ticker, limit: Option[Int]): F[List[Stock]]

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

  override def findWithPriceDeltas(ticker: Ticker, limit: Option[Int]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](
        Aggregate
          .matchBy(Filter.eq(Field.Ticker, ticker))
          .setWindowFields(
            "$" + Field.Ticker,
            Sort.asc(Field.LastUpdatedAt),
            List(WindowOutputFields.shift("prevPrice", "$" + Field.Price, null, -1))
          )
          .set(
            Field.PriceDelta -> Document(
              "$cond" := BsonValue.document(
                "if"   -> BsonValue.document("$eq" := List(BsonValue.string("$prevPrice"), BsonValue.BNull)),
                "then" -> BsonValue.BNull,
                "else" -> BsonValue.document("$subtract" := List("$price", "$prevPrice"))
              )
            )
          )
          .sort(Sort.desc(Field.LastUpdatedAt))
      )
      .all
      .mapList(_.toDomain)
}

object StockRepository:
  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity]("stocks")
      .map(LiveStockRepository[F](_))
