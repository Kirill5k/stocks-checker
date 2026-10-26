package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import io.circe.Codec
import mongo4cats.circe.{MongoJsonCodecs, given}
import mongo4cats.bson.syntax.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{IndexOptions, UpdateOptions}
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.{FinancialSnapshot, Ticker}

trait FinancialHistoryRepository[F[_]]:
  def save(snapshot: FinancialSnapshot): F[Unit]
  def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]]
  def findLatestAll: F[List[FinancialSnapshot]]

final private case class FinancialVintageRef(id: String, observedAt: String) derives Codec.AsObject

final private class LiveFinancialHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, FinancialSnapshot])
    extends FinancialHistoryRepository[F]:
  override def save(snapshot: FinancialSnapshot): F[Unit] =
    for
      _ <- collection.updateOne(
        Filter.eq("id", snapshot.id),
        Update.setOnInsert(snapshot.toBson.asDocument.get),
        UpdateOptions(upsert = true)
      )
      // Frozen rankings carry their own evidence. Retain three recent raw vintages for mapping audits.
      cutoff <- collection
        .aggregate[FinancialVintageRef](
          Aggregate
            .matchBy(Filter.eq("ticker", snapshot.ticker.value))
            .sort(Sort.desc("observedAt").desc("id"))
            .skip(2)
            .limit(1)
            .project(Projection.include(List("id", "observedAt")))
        )
        .first
      _ <- cutoff.traverse_ { last =>
        collection
          .deleteMany(
            Filter.eq("ticker", snapshot.ticker.value) &&
              (Filter.lt("observedAt", last.observedAt) || (Filter.eq("observedAt", last.observedAt) && Filter
                .lt("id", last.id)))
          )
          .void
      }
    yield ()

  override def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]] =
    collection.find(Filter.eq("ticker", ticker.value)).sort(Sort.desc("observedAt")).first

  override def findLatestAll: F[List[FinancialSnapshot]] =
    collection
      .aggregate[FinancialSnapshot](
        Aggregate
          .sort(Sort.desc("observedAt"))
          .addFields(List("rawPayload" -> ""))
          .group("$ticker", Accumulator.first("snapshot", "$$ROOT"))
          .replaceWith("$snapshot")
          .sort(Sort.asc("ticker"))
      )
      .all
      .map(_.toList)

object FinancialHistoryRepository extends MongoJsonCodecs:
  val CollectionName = "financial-history"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[FinancialHistoryRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[FinancialSnapshot](CollectionName)
      _          <- collection.createIndex(Index.ascending("id"), IndexOptions().unique(true))
      _          <- collection.createIndex(Index.ascending("ticker").descending("observedAt"))
      _          <- collection.createIndex(Index.descending("observedAt"))
    yield LiveFinancialHistoryRepository[F](collection.withAddedCodec[FinancialVintageRef])
