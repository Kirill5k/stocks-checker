package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import fs2.Stream
import kirill5k.common.cats.Clock
import kirill5k.common.cats.syntax.applicative.*
import kirill5k.common.syntax.time.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Aggregate, Filter, Projection, Update}
import stockschecker.domain.{Exchange, Security, SecurityFilter, SecurityKind, Ticker}
import stockschecker.repositories.entities.{Entity, SecurityEntity}

import java.time.Instant

trait SecurityRepository[F[_]]:
  def save(security: Security): F[Unit]
  def save(securities: List[Security]): F[Unit]
  def find(ticker: Ticker): F[Option[Security]]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def streamAll: Stream[F, Security]
  def getAllTickers: F[List[Ticker]]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]
  def updateCompanyProfileLastUpdated(ticker: Ticker): F[Unit]
  def updateCompanyProfileLastUpdated(tickers: List[Ticker]): F[Unit]

final private class LiveSecurityRepository[F[_]](
    private val collection: MongoCollection[F, SecurityEntity]
)(using
    F: Concurrent[F],
    clock: Clock[F]
) extends SecurityRepository[F] {

  private object Field:
    val Id                        = "_id"
    val Exchange                  = "exchange"
    val Ticker                    = "ticker"
    val Name                      = "name"
    val Kind                      = "kind"
    val IsActive                  = "isActive"
    val CompanyProfileLastUpdated = "companyProfileLastUpdated"
    val CreatedAt                 = "createdAt"
    val UpdatedAt                 = "updatedAt"

  extension (security: Security)
    private def toUpdateCommand(now: Instant): WriteCommand[Nothing] =
      val id = security.ticker.value
      WriteCommand.UpdateOne(
        Filter.idEq(id),
        Update
          .setOnInsert(Field.Id, id)
          .setOnInsert(Field.CreatedAt, now)
          .set(Field.UpdatedAt, now)
          .set(Field.Ticker, security.ticker)
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

  override def find(ticker: Ticker): F[Option[Security]] =
    collection.find(Filter.idEq(ticker.value)).first.map(_.map(_.toDomain))

  override def findByExchange(exchange: Exchange): F[List[Security]] =
    collection
      .find(Filter.eq(Field.Exchange, exchange))
      .all
      .mapList(_.toDomain)

  override def getAllTickers: F[List[Ticker]] =
    collection.distinct[Ticker]("ticker").all.map(_.toList)

  override def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]] =
    filter.toFilter.flatMap { mongoFilter =>
      collection
        .aggregate[Entity](
          Aggregate
            .matchBy(mongoFilter)
            .limit(limit.getOrElse(Int.MaxValue))
            .project(Projection.include(Field.Id))
        )
        .all
        .mapList(_._id)
    }

  override def updateCompanyProfileLastUpdated(ticker: Ticker): F[Unit] =
    collection.updateOne(Filter.idEq(ticker.value), Update.currentDate(Field.CompanyProfileLastUpdated)).void

  override def updateCompanyProfileLastUpdated(tickers: List[Ticker]): F[Unit] =
    F.whenA(tickers.nonEmpty) {
      collection
        .updateMany(
          Filter.in(Field.Id, tickers.map(_.value)),
          Update.currentDate(Field.CompanyProfileLastUpdated)
        )
        .void
    }

  extension (f: SecurityFilter)
    private def toFilter: F[Filter] = f match
      case SecurityFilter.ExchangeIs(exchange) =>
        Filter.eq(Field.Exchange, exchange).pure
      case SecurityFilter.KindIs(kind) =>
        Filter.eq(Field.Kind, kind).pure
      case SecurityFilter.IsActive(active) =>
        Filter.eq(Field.IsActive, active).pure
      case SecurityFilter.UpdatedWithin(duration) =>
        clock.now.map(currentTime => Filter.gt(Field.UpdatedAt, currentTime.minus(duration)))
      case SecurityFilter.NotUpdatedFor(duration) =>
        clock.now.map(currentTime => Filter.lt(Field.UpdatedAt, currentTime.minus(duration)))
      case SecurityFilter.Composite(filters) =>
        filters.traverse(_.toFilter).map(_.toList.foldLeft(Filter.empty)(_ && _))
}

object SecurityRepository extends MongoJsonCodecs:
  def make[F[_]](database: MongoDatabase[F])(using Concurrent[F], Clock[F]): F[SecurityRepository[F]] =
    database
      .getCollectionWithCodec[SecurityEntity]("securities")
      .map(_.withAddedCodec[Ticker].withAddedCodec[Exchange].withAddedCodec[SecurityKind].withAddedCodec[Entity])
      .map(LiveSecurityRepository[F](_))
