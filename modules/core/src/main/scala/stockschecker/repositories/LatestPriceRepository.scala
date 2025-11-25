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
import mongo4cats.operations.{Filter, Index, Sort, Update}
import stockschecker.domain.{LatestPrice, Ticker}
import stockschecker.repositories.entities.LatestPriceEntity

import java.time.Instant

trait LatestPriceRepository[F[_]]:
  def save(latestPrice: LatestPrice): F[Unit]
  def save(latestPrices: List[LatestPrice]): F[Unit]
  def findLatest(ticker: Ticker): F[Option[LatestPrice]]
  def getAll(ticker: Ticker): F[List[LatestPrice]]

final private class LiveLatestPriceRepository[F[_]](
    private val collection: MongoCollection[F, LatestPriceEntity]
)(using
    F: Monad[F],
    clock: Clock[F]
) extends LatestPriceRepository[F] {

  import LatestPriceRepository.Field

  extension (latestPrice: LatestPrice)
    private def toId: String                   = s"${latestPrice.ticker.value}-${latestPrice.date}"
    private def toUpdate(now: Instant): Update =
      Update
        .setOnInsert(Field.Id, latestPrice.toId)
        .setOnInsert(Field.CreatedAt, now)
        .set(Field.UpdatedAt, now)
        .set(Field.Ticker, latestPrice.ticker)
        .set(Field.Price, latestPrice.price)
        .set(Field.Date, latestPrice.date)

  override def save(latestPrice: LatestPrice): F[Unit] =
    clock.now.flatMap { now =>
      collection.updateOne(Filter.idEq(latestPrice.toId), latestPrice.toUpdate(now), UpdateOptions(upsert = true)).void
    }

  override def save(latestPrices: List[LatestPrice]): F[Unit] =
    clock.now.flatMap { now =>
      val updateOpt = UpdateOptions(upsert = true)
      val updates   = latestPrices.map { lp =>
        WriteCommand.UpdateOne(Filter.idEq(lp.toId), lp.toUpdate(now), updateOpt)
      }
      collection.bulkWrite(updates).void
    }

  override def findLatest(ticker: Ticker): F[Option[LatestPrice]] =
    collection
      .find(Filter.eq(Field.Ticker, ticker))
      .sort(Sort.desc(Field.Date))
      .first
      .mapOpt(_.toDomain)

  override def getAll(ticker: Ticker): F[List[LatestPrice]] =
    collection
      .find(Filter.eq(Field.Ticker, ticker))
      .sort(Sort.desc(Field.Date))
      .all
      .mapList(_.toDomain)
}

object LatestPriceRepository extends MongoJsonCodecs:
  val CollectionName = "latest-prices"
  object Field:
    val Id        = "_id"
    val Ticker    = "ticker"
    val Price     = "price"
    val Date      = "date"
    val CreatedAt = "createdAt"
    val UpdatedAt = "updatedAt"

  def make[F[_]: {Concurrent, Clock}](database: MongoDatabase[F]): F[LatestPriceRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[LatestPriceEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.Ticker))
    yield LiveLatestPriceRepository[F](collection.withAddedCodec[Ticker])
