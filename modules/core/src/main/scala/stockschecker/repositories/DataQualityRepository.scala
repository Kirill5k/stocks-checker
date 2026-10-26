package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.UpdateOptions
import mongo4cats.operations.{Filter, Index, Update}
import stockschecker.domain.{DatasetAvailability, Ticker}
import java.time.Instant

trait DataQualityRepository[F[_]]:
  def record(ticker: Ticker, provider: String, dataset: String, status: String, at: Instant, failure: Option[String]): F[Unit]
  def all: F[List[DatasetAvailability]]

object DataQualityRepository extends MongoJsonCodecs {
  final private case class Record(
      _id: String,
      ticker: Ticker,
      provider: String,
      dataset: String,
      status: String,
      lastAttemptAt: Instant,
      lastSuccessAt: Option[Instant],
      failure: Option[String]
  ) derives Codec.AsObject
  private given MongoCodecProvider[Record] = deriveCirceCodecProvider[Record]

  def make[F[_]: Concurrent](db: MongoDatabase[F]): F[DataQualityRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[Record]("dataset-availability")
      _          <- collection.createIndex(Index.ascending("status"))
    yield new LiveDataQualityRepository[F](collection.withAddedCodec[Ticker])

  final private class LiveDataQualityRepository[F[_]: Concurrent](collection: MongoCollection[F, Record]) extends DataQualityRepository[F] {
    override def record(
        ticker: Ticker,
        provider: String,
        dataset: String,
        status: String,
        at: Instant,
        failure: Option[String]
    ): F[Unit] = {
      val id     = s"$provider:$dataset:${ticker.value}"
      val update = Update
        .setOnInsert("_id", id)
        .set("ticker", ticker)
        .set("provider", provider)
        .set("dataset", dataset)
        .set("status", status)
        .set("lastAttemptAt", at)
        .set("failure", failure)
      val dated = if (status == "available") update.set("lastSuccessAt", at) else update
      collection.updateOne(Filter.idEq(id), dated, UpdateOptions(upsert = true)).void
    }

    override def all: F[List[DatasetAvailability]] = collection.find.all.map(
      _.toList.map(r => DatasetAvailability(r.ticker, r.provider, r.dataset, r.status, r.lastAttemptAt, r.lastSuccessAt, r.failure))
    )
  }
}
