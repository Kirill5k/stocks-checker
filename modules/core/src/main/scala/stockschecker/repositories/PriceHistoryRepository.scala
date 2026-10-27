package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.repositories.entities.{PriceHistoryEntity, PriceHistorySnapshotRef}
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort, Update}
import mongo4cats.models.collection.UpdateOptions
import stockschecker.domain.{DateRange, PriceAdjustment, PriceHistorySnapshot, Ticker}

trait PriceHistoryRepository[F[_]]:
  def save(snapshot: PriceHistorySnapshot): F[Unit]
  def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]]
  def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, range: DateRange): F[Option[PriceHistorySnapshot]]

final private class LivePriceHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, PriceHistoryEntity])
    extends PriceHistoryRepository[F] {
  import PriceHistoryRepository.Field

  override def save(snapshot: PriceHistorySnapshot): F[Unit] = collection
    .updateOne(
      Filter.idEq(snapshot.id),
      Update
        .setOnInsert(Field.id, snapshot.id)
        .setOnInsert(Field.ticker, snapshot.ticker)
        .setOnInsert(Field.adjustment, snapshot.adjustment)
        .setOnInsert(Field.data, snapshot),
      UpdateOptions(upsert = true)
    )
    .void >> retainRecent(snapshot.ticker, snapshot.adjustment)

  // Completed analytics/evaluations retain computed results and provenance IDs, not a permanent candle archive.
  // Raw candles retain three recent vintages plus the broadest cached history; older IDs may no longer resolve.
  // Keep each vintage intact; merging adjusted series across corporate actions is unsafe.
  private def retainRecent(ticker: Ticker, adjustment: PriceAdjustment): F[Unit] = {
    val matching  = Filter.eq(Field.ticker, ticker) && Filter.eq(Field.adjustment, adjustment)
    val reference = Projection.include(Field.id).computed(Field.snapshotFetchedAt, "$" + Field.fetchedAt)
    for
      recent <- collection
        .aggregate[PriceHistorySnapshotRef](
          Aggregate
            .matchBy(matching)
            .sort(Sort.desc(Field.fetchedAt))
            .limit(3)
            .project(reference)
        )
        .all
      broadest <- collection
        .aggregate[PriceHistorySnapshotRef](
          Aggregate
            .matchBy(matching)
            .sort(Sort.asc(Field.rangeFrom).desc(Field.fetchedAt))
            .limit(1)
            .project(reference)
        )
        .first
      retained = recent.map(_._id).toList ++ broadest.map(_._id).toList
      _ <- recent.headOption.traverse_(newest =>
        collection.deleteMany(matching && Filter.nin(Field.id, retained) && Filter.lte(Field.fetchedAt, newest.fetchedAt)).void
      )
    yield ()
  }

  override def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]] =
    collection
      .find(Filter.eq(Field.ticker, ticker) && Filter.eq(Field.adjustment, adjustment))
      .sort(Sort.desc(Field.fetchedAt))
      .first
      .map(_.map(_.data))

  override def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, range: DateRange): F[Option[PriceHistorySnapshot]] =
    collection
      .find(
        Filter.eq(Field.ticker, ticker) && Filter.eq(Field.adjustment, adjustment) &&
          Filter.lte(Field.rangeFrom, range.from.toString) && Filter.gte(Field.rangeThrough, range.through.toString)
      )
      .sort(Sort.desc(Field.fetchedAt))
      .first
      .map(_.map(_.data))
}

object PriceHistoryRepository extends MongoJsonCodecs {
  val CollectionName = "price-observations"

  object Field:
    val id                = "_id"
    val ticker            = "ticker"
    val adjustment        = "adjustment"
    val data              = "data"
    val fetchedAt         = "data.fetchedAt"
    val rangeFrom         = "data.requestedRange.from"
    val rangeThrough      = "data.requestedRange.through"
    val snapshotFetchedAt = "fetchedAt"

  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[PriceHistoryRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[PriceHistoryEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(List(Field.ticker, Field.adjustment, Field.fetchedAt)))
      _          <- collection.createIndex(Index.ascending(List(Field.ticker, Field.adjustment, Field.rangeFrom, Field.fetchedAt)))
    yield new LivePriceHistoryRepository[F](
      collection
        .withAddedCodec[Ticker]
        .withAddedCodec[PriceAdjustment]
        .withAddedCodec[PriceHistorySnapshot]
        .withAddedCodec[PriceHistorySnapshotRef]
    )

}
