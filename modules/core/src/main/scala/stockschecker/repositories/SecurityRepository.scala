package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import fs2.Stream
import kirill5k.common.cats.Clock
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.{Exchange, Security, Ticker}
import stockschecker.repositories.entities.SecurityEntity

import java.time.Instant

trait SecurityRepository[F[_]]:
  def save(security: Security): F[Unit]
  def save(securities: List[Security]): F[Unit]
  def findByTicker(ticker: Ticker): F[Option[Security]]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def streamAll: Stream[F, Security]
  def getAllTickers: F[List[Ticker]]

final private class LiveSecurityRepository[F[_]](
    private val collection: MongoCollection[F, SecurityEntity]
)(using
  F: Concurrent[F],
  clock: Clock[F]
) extends SecurityRepository[F] {

  private object Field:
    val Id       = "_id"
    val Exchange = "exchange"
    val Name     = "name"
    val Kind     = "kind"
    val IsActive = "isActive"
    val createdAt = "createdAt"
    val updatedAt = "updatedAt"

  extension (security: Security)
    private def toUpdateCommand(now: Instant): WriteCommand[Nothing] =
      val id = security.ticker.value
      WriteCommand.UpdateOne(
        Filter.idEq(id),
        Update
          .setOnInsert(Field.Id, id)
          .setOnInsert(Field.createdAt, now)
          .set(Field.updatedAt, now)
          .set(Field.Exchange, security.exchange)
          .set(Field.Name, security.name)
          .set(Field.Kind, security.kind)
          .set(Field.IsActive, security.isActive),
        UpdateOptions(upsert = true)
      )

  override def save(securities: List[Security]): F[Unit] =
    clock.now.flatMap { now =>
      collection.bulkWrite(securities.map(_.toUpdateCommand(now))).void
    }

  override def save(security: Security): F[Unit] =
    clock.now.flatMap { now =>
      collection.bulkWrite(List(security.toUpdateCommand(now))).void
    }

  override def streamAll: Stream[F, Security] =
    collection.find.stream.map(_.toDomain)

  override def findByTicker(ticker: Ticker): F[Option[Security]] =
    collection.find(Filter.idEq(ticker.value)).first.map(_.map(_.toDomain))

  override def findByExchange(exchange: Exchange): F[List[Security]] =
    collection
      .find(Filter.eq(Field.Exchange, exchange))
      .all
      .mapList(_.toDomain)

  override def getAllTickers: F[List[Ticker]] =
    collection.find.all.mapList(_.toDomain.ticker)
}

object SecurityRepository:
  def make[F[_]: {Concurrent, Clock}](database: MongoDatabase[F]): F[SecurityRepository[F]] =
    database
      .getCollectionWithCodec[SecurityEntity]("securities")
      .map(LiveSecurityRepository[F](_))