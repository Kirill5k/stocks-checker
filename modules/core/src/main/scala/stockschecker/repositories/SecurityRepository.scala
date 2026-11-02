package stockschecker.repositories

import stockschecker.domain.ListingStatus
import cats.MonadThrow
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.syntax.applicative.*
import kirill5k.common.syntax.time.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Update}
import stockschecker.domain.{Exchange, Security, SecurityFilter, SecurityKind, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.entities.{Entity, SecurityEntity}

import java.time.Instant

trait SecurityRepository[F[_]]:
  def save(security: Security): F[Unit]
  def save(securities: List[Security]): F[Unit]
  def findVisible(ticker: Ticker): F[Option[Security]]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def getAllTickers: F[List[Ticker]]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]
  def updateCompanyProfileLastUpdated(tickers: List[Ticker]): F[Unit]

  /** Whether the event was applied or replayed; stale requests and absent securities return false. */
  def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant): F[Boolean]
  def reconcile(exchange: Exchange, securities: List[Security]): F[Unit]

final private class LiveSecurityRepository[F[_]](
    private val collection: MongoCollection[F, SecurityEntity]
)(using
    F: MonadThrow[F],
    clock: Clock[F]
) extends SecurityRepository[F] {

  import SecurityRepository.Field

  extension (security: Security)
    private def toUpdateCommand(now: Instant): WriteCommand[Nothing] =
      val id     = security.ticker.value
      var update = Update
        .setOnInsert(Field.Id, id)
        .setOnInsert(Field.CreatedAt, now)
        .setOnInsert(Field.IsActive, true)
        .set(Field.UpdatedAt, now)
        .set(Field.Ticker, security.ticker)
        .set(Field.Exchange, security.exchange)
        .set(Field.Name, security.name)
        .set(Field.Kind, security.kind)
        .set(Field.ListingStatus, ListingStatus.Listed.print)
      update = security.companyProfileLastUpdatedAt.fold(update)(ts => update.set(Field.CompanyProfileLastUpdatedAt, ts))
      WriteCommand.UpdateOne(Filter.idEq(id), update, UpdateOptions(upsert = true))

  override def save(securities: List[Security]): F[Unit] =
    clock.now.flatMap { now =>
      F.whenA(securities.nonEmpty)(collection.bulkWrite(securities.map(_.toUpdateCommand(now))).void)
    }

  override def save(security: Security): F[Unit] =
    clock.now.flatMap { now =>
      collection.bulkWrite(List(security.toUpdateCommand(now))).void
    }

  override def findVisible(ticker: Ticker): F[Option[Security]] =
    collection
      .aggregate[SecurityEntity](
        Aggregate
          .matchBy(Filter.idEq(ticker.value) && StockVisibility.activeSecurity)
          .combinedWith(StockVisibility.withActiveProfile(Field.Id))
      )
      .first
      .mapOpt(_.toDomain)

  override def findByExchange(exchange: Exchange): F[List[Security]] =
    collection
      .aggregate[SecurityEntity](
        Aggregate
          .matchBy(Filter.eq(Field.Exchange, exchange) && StockVisibility.activeSecurity)
          .combinedWith(StockVisibility.withActiveProfile(Field.Id))
      )
      .all
      .mapList(_.toDomain)

  override def getAllTickers: F[List[Ticker]] =
    collection
      .aggregate[Entity](
        Aggregate
          .matchBy(StockVisibility.activeSecurity)
          .combinedWith(StockVisibility.withActiveProfile(Field.Id))
          .project(Projection.include(Field.Id))
      )
      .all
      .mapList(_._id)

  override def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]] =
    filter.toFilter.flatMap { mongoFilter =>
      collection
        .aggregate[Entity](
          Aggregate
            .matchBy(StockVisibility.activeSecurity && mongoFilter)
            .limit(limit.getOrElse(Int.MaxValue))
            .project(Projection.include(Field.Id))
        )
        .all
        .mapList(_._id)
    }

  override def updateCompanyProfileLastUpdated(tickers: List[Ticker]): F[Unit] =
    F.whenA(tickers.nonEmpty) {
      collection
        .updateMany(
          Filter.in(Field.Id, tickers.map(_.value)),
          Update.currentDate(Field.CompanyProfileLastUpdatedAt)
        )
        .void
    }

  override def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant): F[Boolean] =
    val currentRequest = Filter.idEq(ticker.value) &&
      (Filter.isNull(Field.ActiveChangedAt) || Filter.lte(Field.ActiveChangedAt, ActiveChangedAt.encode(requestedAt)))
    val filter = if active then currentRequest && Filter.eq(Field.ListingStatus, ListingStatus.Listed.print) else currentRequest
    collection
      .updateOne(
        filter,
        Update
          .set(Field.IsActive, active)
          .set(Field.ActiveChangedAt, ActiveChangedAt.encode(requestedAt))
          .set(Field.ExclusionReason, Option.when(!active)("user-excluded"))
          .currentDate(Field.UpdatedAt)
      )
      .flatMap { result =>
        if result.getMatchedCount > 0 then true.pure[F]
        else if active then
          collection
            .aggregate[Entity](
              Aggregate
                .matchBy(currentRequest && Filter.eq(Field.ListingStatus, ListingStatus.Delisted.print))
                .project(Projection.include(Field.Id))
            )
            .first
            .flatMap { delisted =>
              F.raiseWhen(delisted.nonEmpty)(AppError.FailedValidation(s"Cannot activate delisted security $ticker")).as(false)
            }
        else false.pure[F]
      }

  override def reconcile(exchange: Exchange, securities: List[Security]): F[Unit] =
    F.whenA(securities.nonEmpty) {
      val listed = securities.map(_.ticker.value)
      for
        _ <- save(securities)
        _ <- collection
          .updateMany(
            Filter.in(Field.Id, listed) && Filter.isNull(Field.ExclusionReason),
            Update.set(Field.IsActive, true)
          )
          .void
        _ <- collection
          .updateMany(
            Filter.eq(Field.Exchange, exchange) && Filter.nin(Field.Id, listed),
            Update.set(Field.ListingStatus, ListingStatus.Delisted.print).set(Field.IsActive, false)
          )
          .void
      yield ()
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
        clock.now.map(now => Filter.gt(Field.UpdatedAt, now.minus(duration)))
      case SecurityFilter.NotUpdatedFor(duration) =>
        clock.now.map(now => Filter.lt(Field.UpdatedAt, now.minus(duration)))
      case SecurityFilter.CompanyProfileNotUpdatedFor(duration) =>
        val isNullOrLt = (ts: Instant) =>
          Filter.isNull(Field.CompanyProfileLastUpdatedAt) || Filter.lt(Field.CompanyProfileLastUpdatedAt, ts)
        clock.now.map(now => isNullOrLt(now.minus(duration)))
      case SecurityFilter.Composite(filters) =>
        filters.traverse(_.toFilter).map(_.toList.foldLeft(Filter.empty)(_ && _))
}

object SecurityRepository extends MongoJsonCodecs:
  val CollectionName = "securities"
  object Field:
    val Id                          = "_id"
    val Exchange                    = "exchange"
    val Ticker                      = "ticker"
    val Name                        = "name"
    val Kind                        = "kind"
    val IsActive                    = "isActive"
    val ActiveChangedAt             = "activeChangedAt"
    val ExclusionReason             = "exclusionReason"
    val ListingStatus               = "listingStatus"
    val CompanyProfileLastUpdatedAt = "companyProfileLastUpdatedAt"
    val CreatedAt                   = "createdAt"
    val UpdatedAt                   = "updatedAt"

  def make[F[_]: {Concurrent, Clock}](database: MongoDatabase[F]): F[SecurityRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[SecurityEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.Ticker))
      _          <- collection.createIndex(Index.ascending(Field.Exchange))
      _          <- collection.createIndex(Index.ascending(Field.Kind))
      _          <- collection.createIndex(Index.ascending(Field.IsActive))
      _          <- collection.createIndex(Index.ascending(Field.ExclusionReason))
      _          <- collection.createIndex(Index.ascending(List(Field.Exchange, Field.Kind, Field.IsActive)))
      _          <- collection.createIndex(Index.ascending(Field.UpdatedAt))
      _          <- collection.createIndex(Index.ascending(Field.CompanyProfileLastUpdatedAt))
      collWithCodecs = collection.withAddedCodec[Ticker].withAddedCodec[Exchange].withAddedCodec[SecurityKind].withAddedCodec[Entity]
    yield LiveSecurityRepository[F](collWithCodecs)
