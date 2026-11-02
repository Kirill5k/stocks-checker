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
import stockschecker.repositories.entities.{Entity, SecurityEntity}
import stockschecker.repositories.queries.StockVisibility

import java.time.Instant

trait SecurityRepository[F[_]]:
  def save(securities: List[Security]): F[Unit]
  def findVisible(ticker: Ticker): F[Option[Security]]
  def findByExchange(exchange: Exchange): F[List[Security]]
  def getAllTickers: F[List[Ticker]]
  def findTickersBy(filter: SecurityFilter, limit: Option[Int]): F[List[Ticker]]
  def updateCompanyProfileLastUpdated(tickers: List[Ticker]): F[Unit]

  /** Equal timestamp replays are accepted so a failed dispatch can resume. */
  def setActive(
      ticker: Ticker,
      active: Boolean,
      requestedAt: Instant,
      reason: String = "user-excluded"
  ): F[SecurityRepository.ActivationResult]
  def countListed(exchange: Exchange): F[Long]
  def reconcile(exchange: Exchange, securities: List[Security]): F[List[Ticker]]
  def pendingPurges: F[List[SecurityRepository.PendingPurge]]
  def acknowledgePurge(ticker: Ticker, requestedAt: Instant): F[Unit]

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
      update = security.companyProfileLastUpdatedAt.fold(update)(ts => update.setOnInsert(Field.CompanyProfileLastUpdatedAt, ts))
      WriteCommand.UpdateOne(Filter.idEq(id), update, UpdateOptions(upsert = true))

  override def save(securities: List[Security]): F[Unit] =
    clock.now.flatMap { now =>
      F.whenA(securities.nonEmpty)(collection.bulkWrite(securities.map(_.toUpdateCommand(now))).void)
    }

  override def findVisible(ticker: Ticker): F[Option[Security]] =
    collection
      .aggregate[SecurityEntity](
        Aggregate
          .matchBy(Filter.idEq(ticker.value) && StockVisibility.activeSecurity)
          .combinedWith(StockVisibility.withProfile(Field.Id))
      )
      .first
      .mapOpt(_.toDomain)

  override def findByExchange(exchange: Exchange): F[List[Security]] =
    collection
      .aggregate[SecurityEntity](
        Aggregate
          .matchBy(Filter.eq(Field.Exchange, exchange) && StockVisibility.activeSecurity)
          .combinedWith(StockVisibility.withProfile(Field.Id))
      )
      .all
      .mapList(_.toDomain)

  override def getAllTickers: F[List[Ticker]] =
    collection
      .aggregate[Entity](
        Aggregate
          .matchBy(StockVisibility.activeSecurity)
          .combinedWith(StockVisibility.withProfile(Field.Id))
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

  override def setActive(ticker: Ticker, active: Boolean, requestedAt: Instant, reason: String): F[SecurityRepository.ActivationResult] =
    import SecurityRepository.ActivationResult
    val currentRequest = Filter.idEq(ticker.value) && ActiveChangedAt.accepts(Field.ActiveChangedAt, requestedAt)
    val filter         = if active then currentRequest && Filter.eq(Field.ListingStatus, ListingStatus.Listed.print) else currentRequest
    val update         = Update
      .set(Field.IsActive, active)
      .set(Field.ActiveChangedAt, ActiveChangedAt.encode(requestedAt))
      .set(Field.ExclusionReason, Option.when(!active)(reason))
      .unset(Field.CompanyProfileLastUpdatedAt)
      .currentDate(Field.UpdatedAt)
    val withPurge = if active then update else update.set(Field.PendingPurgeAt, ActiveChangedAt.encode(requestedAt))
    collection
      .updateOne(filter, withPurge)
      .flatMap { result =>
        if result.getMatchedCount > 0 then ActivationResult.Applied.pure[F]
        else
          collection.find(Filter.idEq(ticker.value)).first.map {
            case None                                                                                       => ActivationResult.Missing
            case Some(security) if security.activeChangedAt.exists(_ > ActiveChangedAt.encode(requestedAt)) => ActivationResult.Stale
            case Some(security) if active && security.listingStatus == ListingStatus.Delisted               => ActivationResult.Delisted
            case _                                                                                          => ActivationResult.Stale
          }
      }

  override def countListed(exchange: Exchange): F[Long] =
    collection.count(Filter.eq(Field.Exchange, exchange) && Filter.eq(Field.ListingStatus, ListingStatus.Listed.print))

  override def reconcile(exchange: Exchange, securities: List[Security]): F[List[Ticker]] =
    if securities.isEmpty then List.empty[Ticker].pure[F]
    else
      val listed  = securities.map(_.ticker.value)
      val missing = Filter.eq(Field.Exchange, exchange) && Filter.nin(Field.Id, listed) &&
        Filter.ne(Field.Id, stockschecker.domain.investment.InvestmentEvaluator.Benchmark.value)
      for
        now      <- clock.now
        delisted <- collection
          .aggregate[Entity](Aggregate.matchBy(missing && StockVisibility.activeSecurity).project(Projection.include(Field.Id)))
          .all
        _ <- save(securities)
        _ <- collection
          .updateMany(Filter.in(Field.Id, listed) && Filter.isNull(Field.ExclusionReason), Update.set(Field.IsActive, true))
          .void
        _ <- collection
          .updateMany(
            missing && StockVisibility.activeSecurity,
            Update
              .set(Field.ListingStatus, ListingStatus.Delisted.print)
              .set(Field.IsActive, false)
              .set(Field.PendingPurgeAt, ActiveChangedAt.encode(now))
              .unset(Field.CompanyProfileLastUpdatedAt)
          )
          .void
        _ <- collection
          .updateMany(
            missing,
            Update
              .set(Field.ListingStatus, ListingStatus.Delisted.print)
              .set(Field.IsActive, false)
              .unset(Field.CompanyProfileLastUpdatedAt)
          )
          .void
      yield delisted.toList.map(_._id)

  override def pendingPurges: F[List[SecurityRepository.PendingPurge]] =
    collection
      .find(Filter.exists(Field.PendingPurgeAt) && Filter.ne(Field.PendingPurgeAt, Option.empty[String]))
      .all
      .map { securities =>
        securities.toList
          .flatMap { security =>
            security.pendingPurgeAt.map(requestedAt => SecurityRepository.PendingPurge(security._id, Instant.parse(requestedAt)))
          }
      }

  override def acknowledgePurge(ticker: Ticker, requestedAt: Instant): F[Unit] =
    collection
      .updateOne(
        Filter.idEq(ticker.value) && Filter.eq(Field.PendingPurgeAt, ActiveChangedAt.encode(requestedAt)),
        Update.unset(Field.PendingPurgeAt)
      )
      .void

  extension (f: SecurityFilter)
    private def toFilter: F[Filter] = f match
      case SecurityFilter.ExchangeIs(exchange) =>
        Filter.eq(Field.Exchange, exchange).pure
      case SecurityFilter.KindIs(kind) =>
        Filter.eq(Field.Kind, kind).pure
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
  final case class PendingPurge(ticker: Ticker, requestedAt: Instant)

  enum ActivationResult:
    case Applied, Stale, Delisted, Missing

  val CollectionName = "securities"
  object Field:
    val Id                          = "_id"
    val Exchange                    = "exchange"
    val Ticker                      = "ticker"
    val Name                        = "name"
    val Kind                        = "kind"
    val IsActive                    = "isActive"
    val ActiveChangedAt             = "activeChangedAt"
    val PendingPurgeAt              = "pendingPurgeAt"
    val ExclusionReason             = "exclusionReason"
    val ListingStatus               = "listingStatus"
    val CompanyProfileLastUpdatedAt = "companyProfileLastUpdatedAt"
    val CreatedAt                   = "createdAt"
    val UpdatedAt                   = "updatedAt"

  def make[F[_]: {Concurrent, Clock}](database: MongoDatabase[F]): F[SecurityRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[SecurityEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.Kind))
      _          <- collection.createIndex(Index.ascending(List(Field.IsActive, Field.ListingStatus)))
      _          <- collection.createIndex(Index.ascending(Field.ExclusionReason))
      _          <- collection.createIndex(Index.ascending(List(Field.Exchange, Field.Kind, Field.IsActive)))
      _          <- collection.createIndex(Index.ascending(Field.UpdatedAt))
      _          <- collection.createIndex(Index.ascending(Field.CompanyProfileLastUpdatedAt))
      collWithCodecs = collection.withAddedCodec[Ticker].withAddedCodec[Exchange].withAddedCodec[SecurityKind].withAddedCodec[Entity]
    yield LiveSecurityRepository[F](collWithCodecs)
