package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.Filter
import mongo4cats.models.collection.ReplaceOptions
import stockschecker.repositories.entities.PriceHistoryEntity
import stockschecker.domain.{DateRange, PriceAdjustment, PriceHistorySnapshot, Ticker}

trait PriceHistoryRepository[F[_]]:
  def save(snapshot: PriceHistorySnapshot): F[Unit]
  def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]]
  def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, range: DateRange): F[Option[PriceHistorySnapshot]]

final private class LivePriceHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, PriceHistoryEntity])
    extends PriceHistoryRepository[F] {
  import PriceHistoryRepository.Field

  private def key(ticker: Ticker, adjustment: PriceAdjustment): String = s"${ticker.value}:${adjustment.value}"

  // Replace a complete vintage atomically; never merge corporate-action vintages.
  override def save(snapshot: PriceHistorySnapshot): F[Unit] =
    val newer = Filter.idEq(key(snapshot.ticker, snapshot.adjustment)) &&
      Filter.lt(Field.FetchedAt, snapshot.fetchedAt) && Filter.ne(Field.SnapshotId, snapshot.id)
    val entity = PriceHistoryEntity(key(snapshot.ticker, snapshot.adjustment), snapshot.ticker, snapshot.adjustment, snapshot)
    collection
      .replaceOne(newer, entity, ReplaceOptions(upsert = true))
      .void
      .handleErrorWith {
        // A concurrent first insert can win with an older timestamp. Retry without upsert so the newer vintage still wins.
        case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 =>
          collection.replaceOne(newer, entity, ReplaceOptions(upsert = false)).void
        case error => Concurrent[F].raiseError(error)
      }

  override def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]] =
    collection
      .find(Filter.idEq(key(ticker, adjustment)))
      .first
      .mapOpt(_.data)

  override def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, range: DateRange): F[Option[PriceHistorySnapshot]] =
    collection
      .find(
        Filter.idEq(key(ticker, adjustment)) &&
          Filter.lte(Field.RangeFrom, range.from.toString) && Filter.gte(Field.RangeThrough, range.through.toString)
      )
      .first
      .mapOpt(_.data)
}

object PriceHistoryRepository extends MongoJsonCodecs {
  val CollectionName = "price-observations"

  object Field:
    val SnapshotId   = "data.id"
    val FetchedAt    = "data.fetchedAt"
    val RangeFrom    = "data.requestedRange.from"
    val RangeThrough = "data.requestedRange.through"

  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[PriceHistoryRepository[F]] =
    db.getCollectionWithCodec[PriceHistoryEntity](CollectionName).map(new LivePriceHistoryRepository[F](_))
}
