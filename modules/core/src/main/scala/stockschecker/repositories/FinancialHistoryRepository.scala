package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import mongo4cats.circe.{MongoJsonCodecs, given}
import mongo4cats.bson.syntax.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{IndexOptions, UpdateOptions}
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Update}
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
        Filter.eq(Field.Id, snapshot.id),
        Update.setOnInsert(snapshot.toBson.asDocument.get),
        UpdateOptions(upsert = true)
      )
      // Frozen rankings carry their own evidence; only the latest canonical snapshot is read here.
      vintages <- collection
        .aggregate[FinancialVintageRef](
          Aggregate
            .matchBy(Filter.eq(Field.Ticker, snapshot.ticker.value))
            .project(Projection.include(List(Field.Id, Field.ObservedAt)))
        )
        .all
      // ISO strings with different fractional precision do not sort chronologically.
      latest   = vintages.maxByOption(vintage => (vintage.observedInstant, vintage.id))
      obsolete = vintages.filterNot(vintage => latest.contains(vintage)).map(_.id).toList
      // Delete only the observed losers; a concurrent refresh may have inserted a newer winner.
      _ <- Concurrent[F].whenA(obsolete.nonEmpty)(collection.deleteMany(Filter.in(Field.Id, obsolete)).void)
    yield ()

  override def findLatest(ticker: Ticker): F[Option[FinancialSnapshot]] =
    collection
      .aggregate[FinancialSnapshot](
        Aggregate.matchBy(Filter.eq(Field.Ticker, ticker.value)).project(Projection.exclude(Field.RawPayload))
      )
      .all
      .map(_.maxByOption(snapshot => (snapshot.observedAt, snapshot.id)))

  override def findLatestAll: F[List[FinancialSnapshot]] =
    collection
      .aggregate[FinancialSnapshot](Aggregate.project(Projection.exclude(Field.RawPayload)))
      .all
      .map(_.groupBy(_.ticker).values.map(_.maxBy(snapshot => (snapshot.observedAt, snapshot.id))).toList.sortBy(_.ticker.value))

object FinancialHistoryRepository extends MongoJsonCodecs:
  val CollectionName = "financial-history"

  object Field:
    val Id         = "id"
    val Ticker     = "ticker"
    val ObservedAt = "observedAt"
    val RawPayload = "rawPayload"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[FinancialHistoryRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[FinancialSnapshot](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.Id), IndexOptions().unique(true))
      _          <- collection.createIndex(Index.ascending(Field.Ticker).descending(Field.ObservedAt))
      _          <- collection.createIndex(Index.descending(Field.ObservedAt))
    yield LiveFinancialHistoryRepository[F](collection.withAddedCodec[FinancialVintageRef])
