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
import stockschecker.repositories.entities.FinancialVintageRef

trait FinancialHistoryRepository[F[_]]:
  def save(snapshot: FinancialSnapshot): F[Unit]
  def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]]
  def findLatestAll: F[List[FinancialSnapshot]]

final private class LiveFinancialHistoryRepository[F[_]: Concurrent](collection: MongoCollection[F, FinancialSnapshot])
    extends FinancialHistoryRepository[F]:
  import FinancialHistoryRepository.Field

  override def save(snapshot: FinancialSnapshot): F[Unit] =
    for
      _ <- collection.updateOne(
        Filter.eq(Field.id, snapshot.id),
        Update.setOnInsert(snapshot.toBson.asDocument.get),
        UpdateOptions(upsert = true)
      )
      // Frozen rankings carry their own evidence. Retain three recent raw vintages for mapping audits.
      cutoff <- collection
        .aggregate[FinancialVintageRef](
          Aggregate
            .matchBy(Filter.eq(Field.ticker, snapshot.ticker.value))
            .sort(Sort.desc(Field.observedAt).desc(Field.id))
            .skip(2)
            .limit(1)
            .project(Projection.include(List(Field.id, Field.observedAt)))
        )
        .first
      _ <- cutoff.traverse_ { last =>
        collection
          .deleteMany(
            Filter.eq(Field.ticker, snapshot.ticker.value) &&
              (Filter.lt(Field.observedAt, last.observedAt) || (Filter.eq(Field.observedAt, last.observedAt) && Filter
                .lt(Field.id, last.id)))
          )
          .void
      }
    yield ()

  override def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]] =
    collection.find(Filter.eq(Field.ticker, ticker.value)).sort(Sort.desc(Field.observedAt)).first

  override def findLatestAll: F[List[FinancialSnapshot]] =
    collection
      .aggregate[FinancialSnapshot](
        Aggregate
          .sort(Sort.desc(Field.observedAt))
          .addFields(List(Field.rawPayload -> ""))
          .group("$" + Field.ticker, Accumulator.first(Field.snapshot, "$$ROOT"))
          .replaceWith("$" + Field.snapshot)
          .sort(Sort.asc(Field.ticker))
      )
      .all
      .map(_.toList)

object FinancialHistoryRepository extends MongoJsonCodecs:
  val CollectionName = "financial-history"

  object Field:
    val id         = "id"
    val ticker     = "ticker"
    val observedAt = "observedAt"
    val rawPayload = "rawPayload"
    val snapshot   = "snapshot"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[FinancialHistoryRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[FinancialSnapshot](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.id), IndexOptions().unique(true))
      _          <- collection.createIndex(Index.ascending(Field.ticker).descending(Field.observedAt))
      _          <- collection.createIndex(Index.descending(Field.observedAt))
    yield LiveFinancialHistoryRepository[F](collection.withAddedCodec[FinancialVintageRef])
