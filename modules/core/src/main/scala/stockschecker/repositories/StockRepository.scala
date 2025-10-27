package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import com.mongodb.client.model.WindowOutputFields
import fs2.Stream
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.syntax.*
import mongo4cats.bson.{BsonValue, Document}
import mongo4cats.circe.deriveJsonBsonValueEncoder
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Aggregate, Filter, Sort, Update}
import stockschecker.domain.{StockQuote, Ticker}
import stockschecker.repositories.entities.StockQuoteEntity

trait StockRepository[F[_]]:
  def getAllTickers: F[List[Ticker]]
  def save(quote: StockQuote): F[Unit]
  def save(quotes: List[StockQuote]): F[Unit]
  def streamAll: Stream[F, StockQuote]
  def find(ticker: Ticker, limit: Option[Int]): F[List[StockQuote]]
  def findWithPriceDeltas(ticker: Ticker, limit: Option[Int]): F[List[StockQuote]]

final private class LiveStockRepository[F[_]: Concurrent](
    private val collection: MongoCollection[F, StockQuoteEntity]
) extends StockRepository[F] {

  private object Field:
    val Id            = "_id"
    val Ticker        = "ticker"
    val QuotedAt      = "quotedAt"
    val Price         = "price"
    val PreviousClose = "previousClose"
    val ChangeAmount  = "changeAmount"
    val ChangePercent = "changePercent"
    val Volume        = "volume"
    val DayHigh       = "dayHigh"
    val DayLow        = "dayLow"
    val CreatedAt     = "createdAt"

  extension (quote: StockQuote)
    private def id: String = s"${quote.ticker}.${quote.quotedAt.toString.substring(0, 10)}"
    private def toUpdateCommand: WriteCommand[Nothing] =
      val id = quote.id
      WriteCommand.UpdateOne(
        Filter.idEq(id),
        Update
          .setOnInsert(Field.Id, id)
          .setOnInsert(Field.Ticker, quote.ticker)
          .setOnInsert(Field.CreatedAt, quote.createdAt)
          .set(Field.Price, quote.price)
          .set(Field.QuotedAt, quote.quotedAt)
          .set(Field.PreviousClose, quote.previousClose)
          .set(Field.ChangeAmount, quote.changeAmount)
          .set(Field.ChangePercent, quote.changePercent)
          .set(Field.Volume, quote.volume)
          .set(Field.DayHigh, quote.dayHigh)
          .set(Field.DayLow, quote.dayLow),
        UpdateOptions(upsert = true)
      )

  override def save(quotes: List[StockQuote]): F[Unit] =
    collection.bulkWrite(quotes.map(_.toUpdateCommand)).void

  override def save(quote: StockQuote): F[Unit] =
    collection.bulkWrite(List(quote.toUpdateCommand)).void

  override def streamAll: Stream[F, StockQuote] =
    collection.find.stream.map(_.toDomain)

  override def find(ticker: Ticker, limit: Option[Int]): F[List[StockQuote]] =
    collection
      .find(Filter.eq(Field.Ticker, ticker))
      .sortByDesc(Field.QuotedAt)
      .limit(limit.getOrElse(Int.MaxValue))
      .all
      .mapList(_.toDomain)

  override def findWithPriceDeltas(ticker: Ticker, limit: Option[Int]): F[List[StockQuote]] =
    val prevPriceStr = BsonValue.string("$prevPrice")
    val priceStr = BsonValue.string("$price")
    val nullVal = BsonValue.Null

    val subtractPrices: BsonValue = Document("$subtract" -> List[BsonValue](priceStr, prevPriceStr))
    val dividePrices: BsonValue = Document("$divide" -> List[BsonValue](subtractPrices, prevPriceStr))
    val multiplyBy100: BsonValue = Document("$multiply" -> List[BsonValue](dividePrices, BsonValue.int(100)))

    collection
      .aggregate[StockQuoteEntity](
        Aggregate
          .matchBy(Filter.eq(Field.Ticker, ticker))
          .setWindowFields(
            "$" + Field.Ticker,
            Sort.asc(Field.QuotedAt),
            List(WindowOutputFields.shift("prevPrice", "$" + Field.Price, null, -1))
          )
          .set(
            Field.ChangeAmount -> Document(
              "$cond" -> Document(
                "if"   -> Document("$eq" -> List[BsonValue](prevPriceStr, nullVal)),
                "then" -> nullVal,
                "else" -> subtractPrices
              )
            )
          )
          .set(
            Field.ChangePercent -> Document(
              "$cond" -> Document(
                "if" -> Document("$eq" -> List[BsonValue](prevPriceStr, nullVal)),
                "then" -> nullVal,
                "else" -> multiplyBy100
              )
            )
          )
          .set(Field.PreviousClose -> "$prevPrice")
          .sort(Sort.desc(Field.QuotedAt))
      )
      .all
      .mapList(_.toDomain)

  override def getAllTickers: F[List[Ticker]] =
    collection.distinct[Ticker](Field.Ticker).all.map(_.toList)
}

object StockRepository:
  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockQuoteEntity]("stock_quotes")
      .map(LiveStockRepository[F](_))
