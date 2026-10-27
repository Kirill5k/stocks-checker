package stockschecker.repositories

import stockschecker.domain.DatasetStatus
import stockschecker.domain.Provider
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.repositories.entities.DatasetAvailabilityEntity
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.UpdateOptions
import mongo4cats.operations.{Filter, Index, Update}
import stockschecker.domain.{DatasetAvailability, Ticker}
import java.time.Instant

trait DataQualityRepository[F[_]]:
  def record(ticker: Ticker, provider: Provider, dataset: String, status: DatasetStatus, at: Instant, failure: Option[String]): F[Unit]
  def all: F[List[DatasetAvailability]]

final private class LiveDataQualityRepository[F[_]: Concurrent](collection: MongoCollection[F, DatasetAvailabilityEntity])
    extends DataQualityRepository[F] {
  import DataQualityRepository.Field

  override def record(
      ticker: Ticker,
      provider: Provider,
      dataset: String,
      status: DatasetStatus,
      at: Instant,
      failure: Option[String]
  ): F[Unit] = {
    val id     = s"${provider.print}:$dataset:${ticker.value}"
    val update = Update
      .setOnInsert(Field.id, id)
      .set(Field.ticker, ticker)
      .set(Field.provider, provider.print)
      .set(Field.dataset, dataset)
      .set(Field.status, status.print)
      .set(Field.lastAttemptAt, at)
      .set(Field.failure, failure)
    val dated = if (status == DatasetStatus.Available) update.set(Field.lastSuccessAt, at) else update
    collection.updateOne(Filter.idEq(id), dated, UpdateOptions(upsert = true)).void
  }

  override def all: F[List[DatasetAvailability]] = collection.find.all.map(
    _.toList.map(_.toDomain)
  )
}

object DataQualityRepository extends MongoJsonCodecs {
  val CollectionName = "dataset-availability"

  object Field:
    val id            = "_id"
    val ticker        = "ticker"
    val provider      = "provider"
    val dataset       = "dataset"
    val status        = "status"
    val lastAttemptAt = "lastAttemptAt"
    val lastSuccessAt = "lastSuccessAt"
    val failure       = "failure"

  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[DataQualityRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[DatasetAvailabilityEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.status))
    yield new LiveDataQualityRepository[F](collection.withAddedCodec[Ticker])

}
