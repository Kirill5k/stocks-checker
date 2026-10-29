package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.ReplaceOptions
import mongo4cats.operations.Filter
import stockschecker.domain.FinancialSnapshot
import stockschecker.repositories.entities.FinancialSnapshotEntity

trait FinancialHistoryRepository[F[_]]:
  def save(snapshot: FinancialSnapshot): F[Unit]

final private class LiveFinancialHistoryRepository[F[_]](
    collection: MongoCollection[F, FinancialSnapshotEntity]
)(using
    F: Concurrent[F]
) extends FinancialHistoryRepository[F]:
  import FinancialHistoryRepository.Field

  override def save(snapshot: FinancialSnapshot): F[Unit] =
    val newer  = Filter.idEq(snapshot.ticker.value) && Filter.lt(Field.ObservedAt, snapshot.observedAt)
    val entity = FinancialSnapshotEntity.from(snapshot)
    collection
      .replaceOne(newer, entity, ReplaceOptions(upsert = true))
      .void
      .handleErrorWith {
        // A concurrent first insert can win with an older timestamp. Retry without upsert so the newer vintage still wins.
        case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 =>
          collection.replaceOne(newer, entity, ReplaceOptions(upsert = false)).void
        case error => F.raiseError(error)
      }

object FinancialHistoryRepository extends MongoJsonCodecs:
  val CollectionName = "financial-history"

  object Field:
    val ObservedAt = "observedAt"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[FinancialHistoryRepository[F]] =
    database
      .getCollectionWithCodec[FinancialSnapshotEntity](CollectionName)
      .map(LiveFinancialHistoryRepository[F](_))
