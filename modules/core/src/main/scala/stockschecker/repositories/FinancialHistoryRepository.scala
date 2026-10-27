package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import mongo4cats.circe.{MongoJsonCodecs, given}
import mongo4cats.bson.syntax.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{IndexOptions, UpdateOptions}
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.{FinancialSnapshot, Ticker}
import stockschecker.repositories.entities.{FinancialHistoryEntity, FinancialVintageRef}

trait FinancialHistoryRepository[F[_]]:
  def save(snapshot: FinancialSnapshot, rawPayload: String): F[Unit]
  def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]]
  def findLatestAll: F[List[FinancialSnapshot]]

final private class LiveFinancialHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, FinancialSnapshot])
    extends FinancialHistoryRepository[F]:
  import FinancialHistoryRepository.Field

  override def save(snapshot: FinancialSnapshot, rawPayload: String): F[Unit] =
    for
      _ <- collection.updateOne(
        Filter.eq(Field.Id, snapshot.id),
        Update.setOnInsert(FinancialHistoryEntity(snapshot, rawPayload).toBson.asDocument.get),
        UpdateOptions(upsert = true)
      )
      // Frozen rankings carry their own evidence. Retain three recent raw vintages for mapping audits.
      cutoff <- collection
        .aggregate[FinancialVintageRef](
          Aggregate
            .matchBy(Filter.eq(Field.Ticker, snapshot.ticker.value))
            .sort(Sort.desc(Field.ObservedAt).desc(Field.Id))
            .skip(2)
            .limit(1)
            .project(Projection.include(List(Field.Id, Field.ObservedAt)))
        )
        .first
      _ <- cutoff.traverse_ { last =>
        collection
          .deleteMany(
            Filter.eq(Field.Ticker, snapshot.ticker.value) &&
              (Filter.lt(Field.ObservedAt, last.observedAt) || (Filter.eq(Field.ObservedAt, last.observedAt) && Filter
                .lt(Field.Id, last.id)))
          )
          .void
      }
    yield ()

  override def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]] =
    collection.find(Filter.eq(Field.Ticker, ticker.value)).sort(Sort.desc(Field.ObservedAt)).first

  override def findLatestAll: F[List[FinancialSnapshot]] =
    collection
      .aggregate[FinancialSnapshot](
        Aggregate
          .sort(Sort.desc(Field.ObservedAt))
          .group("$" + Field.Ticker, Accumulator.first(Field.Snapshot, "$$ROOT"))
          .replaceWith("$" + Field.Snapshot)
          .sort(Sort.asc(Field.Ticker))
      )
      .all
      .map(_.toList)

object FinancialHistoryRepository extends MongoJsonCodecs:
  val CollectionName = "financial-history"

  object Field:
    val Id         = "id"
    val Ticker     = "ticker"
    val ObservedAt = "observedAt"
    val RawPayload = "rawPayload"
    val Snapshot   = "snapshot"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[FinancialHistoryRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[FinancialSnapshot](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.Id), IndexOptions().unique(true))
      _          <- collection.createIndex(Index.ascending(Field.Ticker).descending(Field.ObservedAt))
      _          <- collection.createIndex(Index.descending(Field.ObservedAt))
    yield LiveFinancialHistoryRepository[F](collection.withAddedCodec[FinancialVintageRef])
