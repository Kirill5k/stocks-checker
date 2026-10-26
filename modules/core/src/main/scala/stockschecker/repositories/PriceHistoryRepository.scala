package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort, Update}
import mongo4cats.models.collection.UpdateOptions
import stockschecker.domain.{PriceAdjustment, PriceHistorySnapshot, Ticker}
import java.time.LocalDate

trait PriceHistoryRepository[F[_]]:
  def save(snapshot: PriceHistorySnapshot): F[Unit]
  def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]]
  def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, from: LocalDate): F[Option[PriceHistorySnapshot]]

object PriceHistoryRepository extends MongoJsonCodecs {
  final private case class Record(_id: String, ticker: Ticker, adjustment: PriceAdjustment, data: PriceHistorySnapshot)
      derives Codec.AsObject
  final private case class SnapshotRef(_id: String, fetchedAt: String) derives Codec.AsObject
  private given MongoCodecProvider[SnapshotRef] = deriveCirceCodecProvider[SnapshotRef]
  private given MongoCodecProvider[Record]      = deriveCirceCodecProvider[Record]

  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[PriceHistoryRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[Record]("price-observations")
      _          <- collection.createIndex(Index.ascending(List("ticker", "adjustment", "data.fetchedAt")))
      _          <- collection.createIndex(Index.ascending(List("ticker", "adjustment", "data.requestedRange.from", "data.fetchedAt")))
    yield new LivePriceHistoryRepository[F](
      collection.withAddedCodec[Ticker].withAddedCodec[PriceAdjustment].withAddedCodec[PriceHistorySnapshot].withAddedCodec[SnapshotRef]
    )

  final private class LivePriceHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, Record])
      extends PriceHistoryRepository[F] {
    override def save(snapshot: PriceHistorySnapshot): F[Unit] = collection
      .updateOne(
        Filter.idEq(snapshot.id),
        Update
          .setOnInsert("_id", snapshot.id)
          .setOnInsert("ticker", snapshot.ticker)
          .setOnInsert("adjustment", snapshot.adjustment)
          .setOnInsert("data", snapshot),
        UpdateOptions(upsert = true)
      )
      .void >> retainRecent(snapshot.ticker, snapshot.adjustment)

    // Completed analytics/evaluations retain computed results and provenance IDs, not a permanent candle archive.
    // Raw candles retain three recent vintages plus the broadest cached history; older IDs may no longer resolve.
    // Keep each vintage intact; merging adjusted series across corporate actions is unsafe.
    private def retainRecent(ticker: Ticker, adjustment: PriceAdjustment): F[Unit] = {
      val matching  = Filter.eq("ticker", ticker) && Filter.eq("adjustment", adjustment)
      val reference = Projection.include("_id").computed("fetchedAt", "$data.fetchedAt")
      for
        recent <- collection
          .aggregate[SnapshotRef](
            Aggregate
              .matchBy(matching)
              .sort(Sort.desc("data.fetchedAt"))
              .limit(3)
              .project(reference)
          )
          .all
        broadest <- collection
          .aggregate[SnapshotRef](
            Aggregate
              .matchBy(matching)
              .sort(Sort.asc("data.requestedRange.from").desc("data.fetchedAt"))
              .limit(1)
              .project(reference)
          )
          .first
        retained = recent.map(_._id).toList ++ broadest.map(_._id).toList
        _ <- recent.headOption.traverse_(newest =>
          collection.deleteMany(matching && Filter.nin("_id", retained) && Filter.lte("data.fetchedAt", newest.fetchedAt)).void
        )
      yield ()
    }

    override def latest(ticker: Ticker, adjustment: PriceAdjustment): F[Option[PriceHistorySnapshot]] =
      collection
        .find(Filter.eq("ticker", ticker) && Filter.eq("adjustment", adjustment))
        .sort(Sort.desc("data.fetchedAt"))
        .first
        .map(_.map(_.data))

    override def latestCovering(ticker: Ticker, adjustment: PriceAdjustment, from: LocalDate): F[Option[PriceHistorySnapshot]] =
      collection
        .find(Filter.eq("ticker", ticker) && Filter.eq("adjustment", adjustment) && Filter.lte("data.requestedRange.from", from.toString))
        .sort(Sort.desc("data.fetchedAt"))
        .first
        .map(_.map(_.data))
  }
}
