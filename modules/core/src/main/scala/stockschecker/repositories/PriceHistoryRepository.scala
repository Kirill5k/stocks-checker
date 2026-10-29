package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.repositories.entities.{PriceHistoryEntity, PriceHistorySnapshotRef}
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Projection, Update}
import mongo4cats.models.collection.UpdateOptions
import stockschecker.domain.{DateRange, PriceAdjustment, PriceHistorySnapshot, Ticker}

import java.time.Instant
import scala.util.Try

trait PriceHistoryRepository[F[_]]:
  def save(snapshot: PriceHistorySnapshot): F[Unit]
  def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]]
  def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, range: DateRange): F[Option[PriceHistorySnapshot]]

final private class LivePriceHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, PriceHistoryEntity])
    extends PriceHistoryRepository[F] {
  import PriceHistoryRepository.Field

  private def key(ticker: Ticker, adjustment: PriceAdjustment): String = s"${ticker.value}:${adjustment.value}"

  // A stable cache slot contains one complete adjustment vintage. Never merge corporate-action vintages.
  override def save(snapshot: PriceHistorySnapshot): F[Unit] = {
    val matching = Filter.idEq(key(snapshot.ticker, snapshot.adjustment))
    val metadata = Aggregate
      .matchBy(matching)
      .project(Projection.computed("id", "$" + Field.SnapshotId).computed("fetchedAt", "$" + Field.FetchedAt).excludeId)
    collection
      .aggregate[PriceHistorySnapshotRef](metadata)
      .first
      .flatMap {
        case Some(current) if current.id == snapshot.id =>
          Concurrent[F].unit
        case Some(current) =>
          // Compare Instants in Scala: ISO strings with different fractional precision do not sort chronologically.
          Concurrent[F].fromTry(Try(Instant.parse(current.fetchedAt))).flatMap { fetchedAt =>
            if !fetchedAt.isBefore(snapshot.fetchedAt) then Concurrent[F].unit
            else
              val unchanged = matching && Filter.eq(Field.SnapshotId, current.id) && Filter.eq(Field.FetchedAt, current.fetchedAt)
              collection.updateOne(unchanged, Update.set(Field.Data, snapshot)).flatMap { result =>
                if result.getMatchedCount == 0 then save(snapshot) else Concurrent[F].unit
              }
          }
        case None =>
          val insert = Update
            .setOnInsert(Field.Id, key(snapshot.ticker, snapshot.adjustment))
            .setOnInsert(Field.Ticker, snapshot.ticker)
            .setOnInsert(Field.Adjustment, snapshot.adjustment)
            .setOnInsert(Field.Data, snapshot)
          collection
            .updateOne(matching, insert, UpdateOptions(upsert = true))
            .flatMap(result => if result.getMatchedCount == 0 then Concurrent[F].unit else save(snapshot))
            .handleErrorWith {
              // Another refresh can insert this slot first; compare its vintage before attempting an update.
              case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 => save(snapshot)
              case error                                                                     => Concurrent[F].raiseError(error)
            }
      }
  }

  override def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]] =
    collection
      .find(Filter.idEq(key(ticker, adjustment)))
      .first
      .map(_.map(_.data))

  override def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, range: DateRange): F[Option[PriceHistorySnapshot]] =
    collection
      .find(
        Filter.idEq(key(ticker, adjustment)) &&
          Filter.lte(Field.RangeFrom, range.from.toString) &&
          Filter.gte(Field.RangeThrough, range.through.toString)
      )
      .first
      .map(_.map(_.data))
}

object PriceHistoryRepository extends MongoJsonCodecs {
  val CollectionName = "price-observations"

  object Field:
    val Id           = "_id"
    val Ticker       = "ticker"
    val Adjustment   = "adjustment"
    val Data         = "data"
    val SnapshotId   = "data.id"
    val FetchedAt    = "data.fetchedAt"
    val RangeFrom    = "data.requestedRange.from"
    val RangeThrough = "data.requestedRange.through"

  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[PriceHistoryRepository[F]] =
    db.getCollectionWithCodec[PriceHistoryEntity](CollectionName).map { collection =>
      new LivePriceHistoryRepository[F](
        collection
          .withAddedCodec[Ticker]
          .withAddedCodec[PriceAdjustment]
          .withAddedCodec[PriceHistorySnapshot]
          .withAddedCodec[PriceHistorySnapshotRef]
      )
    }

}
