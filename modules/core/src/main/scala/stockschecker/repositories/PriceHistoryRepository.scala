package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Index, Sort, Update}
import mongo4cats.models.collection.UpdateOptions
import stockschecker.domain.{PriceHistorySnapshot, Ticker}
import java.time.LocalDate

trait PriceHistoryRepository[F[_]]:
  def save(snapshot: PriceHistorySnapshot): F[Unit]
  def latest(ticker: Ticker, adjustment: String): F[Option[PriceHistorySnapshot]]
  def latestCovering(ticker: Ticker, adjustment: String, from: LocalDate): F[Option[PriceHistorySnapshot]]

object PriceHistoryRepository extends MongoJsonCodecs {
  private final case class Record(_id: String, ticker: Ticker, adjustment: String, data: PriceHistorySnapshot) derives Codec.AsObject
  private given MongoCodecProvider[Record] = deriveCirceCodecProvider[Record]
  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[PriceHistoryRepository[F]] =
    db.getCollectionWithCodec[Record]("price-observations").flatMap { raw =>
      val collection = raw.withAddedCodec[Ticker].withAddedCodec[PriceHistorySnapshot]
      collection.createIndex(Index.ascending(List("ticker", "adjustment"))).as(new PriceHistoryRepository[F] {
        def save(s: PriceHistorySnapshot): F[Unit] = collection.updateOne(Filter.idEq(s.id),
          Update.setOnInsert("_id", s.id).setOnInsert("ticker", s.ticker).setOnInsert("adjustment", s.adjustment).setOnInsert("data", s),
          UpdateOptions(upsert = true)).void
        def latest(ticker: Ticker, adjustment: String): F[Option[PriceHistorySnapshot]] =
          collection.find(Filter.eq("ticker", ticker) && Filter.eq("adjustment", adjustment)).sort(Sort.desc("data.fetchedAt")).first.map(_.map(_.data))
        def latestCovering(ticker: Ticker, adjustment: String, from: LocalDate): F[Option[PriceHistorySnapshot]] =
          collection.find(Filter.eq("ticker", ticker) && Filter.eq("adjustment", adjustment) && Filter.lte("data.requestedFrom", from.toString))
            .sort(Sort.desc("data.fetchedAt")).first.map(_.map(_.data))
      })
    }
}
