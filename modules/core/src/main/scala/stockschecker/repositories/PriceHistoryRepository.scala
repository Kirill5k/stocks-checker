package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort, Update}
import mongo4cats.models.collection.UpdateOptions
import stockschecker.domain.{PriceHistorySnapshot, Ticker}
import java.time.LocalDate

trait PriceHistoryRepository[F[_]]:
  def save(snapshot: PriceHistorySnapshot): F[Unit]
  def latest(ticker: Ticker, adjustment: String): F[Option[PriceHistorySnapshot]]
  def latestCovering(ticker: Ticker, adjustment: String, from: LocalDate): F[Option[PriceHistorySnapshot]]

object PriceHistoryRepository extends MongoJsonCodecs {
  final private case class Record(_id: String, ticker: Ticker, adjustment: String, data: PriceHistorySnapshot) derives Codec.AsObject
  final private case class SnapshotRef(_id: String, fetchedAt: String) derives Codec.AsObject
  private given MongoCodecProvider[SnapshotRef]                                  = deriveCirceCodecProvider[SnapshotRef]
  private given MongoCodecProvider[Record]                                       = deriveCirceCodecProvider[Record]
  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[PriceHistoryRepository[F]] =
    db.getCollectionWithCodec[Record]("price-observations").flatMap { raw =>
      val collection = raw.withAddedCodec[Ticker].withAddedCodec[PriceHistorySnapshot].withAddedCodec[SnapshotRef]
      (collection.createIndex(Index.ascending(List("ticker", "adjustment", "data.fetchedAt"))) >>
        collection.createIndex(Index.ascending(List("ticker", "adjustment", "data.requestedFrom", "data.fetchedAt"))))
        .as(new PriceHistoryRepository[F] {
          def save(s: PriceHistorySnapshot): F[Unit] = collection
            .updateOne(
              Filter.idEq(s.id),
              Update
                .setOnInsert("_id", s.id)
                .setOnInsert("ticker", s.ticker)
                .setOnInsert("adjustment", s.adjustment)
                .setOnInsert("data", s),
              UpdateOptions(upsert = true)
            )
            .void >> retainRecent(s.ticker, s.adjustment)
          // Completed analytics/evaluations retain computed results and provenance IDs, not a permanent candle archive.
          // Raw candles retain three recent vintages plus the broadest cached history; older IDs may no longer resolve.
          // Keep each vintage intact; merging adjusted series across corporate actions is unsafe.
          private def retainRecent(ticker: Ticker, adjustment: String): F[Unit] = {
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
                    .sort(Sort.asc("data.requestedFrom").desc("data.fetchedAt"))
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
          def latest(ticker: Ticker, adjustment: String): F[Option[PriceHistorySnapshot]] =
            collection
              .find(Filter.eq("ticker", ticker) && Filter.eq("adjustment", adjustment))
              .sort(Sort.desc("data.fetchedAt"))
              .first
              .map(_.map(_.data))
          def latestCovering(ticker: Ticker, adjustment: String, from: LocalDate): F[Option[PriceHistorySnapshot]] =
            collection
              .find(Filter.eq("ticker", ticker) && Filter.eq("adjustment", adjustment) && Filter.lte("data.requestedFrom", from.toString))
              .sort(Sort.desc("data.fetchedAt"))
              .first
              .map(_.map(_.data))
        })
    }
}
