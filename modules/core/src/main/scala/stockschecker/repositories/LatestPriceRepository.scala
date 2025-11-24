package stockschecker.repositories

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.{LatestPrice, Ticker}
import stockschecker.repositories.entities.LatestPriceEntity

import java.time.Instant

trait LatestPriceRepository[F[_]]:
  def save(latestPrice: LatestPrice): F[Unit]
  def save(latestPrices: List[LatestPrice]): F[Unit]
  def find(ticker: Ticker): F[Option[LatestPrice]]

final private class LiveLatestPriceRepository[F[_]](
    private val collection: MongoCollection[F, LatestPriceEntity]
)(using
    F: Monad[F],
    clock: Clock[F]
) extends LatestPriceRepository[F] {

  private object Field:
    val Id        = "_id"
    val Ticker    = "ticker"
    val Price     = "price"
    val Date      = "date"
    val CreatedAt = "createdAt"
    val UpdatedAt = "updatedAt"

  extension (latestPrice: LatestPrice)
    private def toUpdate(now: Instant): Update =
      Update
        .setOnInsert(Field.Id, latestPrice.ticker)
        .setOnInsert(Field.CreatedAt, now)
        .set(Field.UpdatedAt, now)
        .set(Field.Ticker, latestPrice.ticker)
        .set(Field.Price, latestPrice.price)
        .set(Field.Date, latestPrice.date)

  override def save(latestPrice: LatestPrice): F[Unit] =
    clock.now.flatMap { now =>
      collection.updateOne(Filter.idEq(latestPrice.ticker), latestPrice.toUpdate(now), UpdateOptions(upsert = true)).void
    }

  override def save(latestPrices: List[LatestPrice]): F[Unit] =
    clock.now.flatMap { now =>
      val updates = latestPrices.map { lp =>
        WriteCommand.UpdateOne(
          Filter.idEq(lp.ticker),
          lp.toUpdate(now),
          UpdateOptions(upsert = true)
        )
      }
      collection.bulkWrite(updates).void
    }

  override def find(ticker: Ticker): F[Option[LatestPrice]] =
    collection.find(Filter.idEq(ticker.value)).first.mapOpt(_.toDomain)
}

object LatestPriceRepository extends MongoJsonCodecs:
  val CollectionName = "latest-prices"

  def make[F[_]: {Concurrent, Clock}](database: MongoDatabase[F]): F[LatestPriceRepository[F]] =
    database
      .getCollectionWithCodec[LatestPriceEntity](CollectionName)
      .map(_.withAddedCodec[Ticker])
      .map(LiveLatestPriceRepository[F](_))

