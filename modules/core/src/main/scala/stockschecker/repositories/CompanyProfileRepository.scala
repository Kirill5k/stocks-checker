package stockschecker.repositories

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Projection, Sort, Update}
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.entities.{CompanyProfileEntity, Entity}
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}

import java.time.Instant

trait CompanyProfileRepository[F[_]]:
  def save(cp: CompanyProfile): F[Unit]
  def save(cps: List[CompanyProfile]): F[Unit]
  def find(ticker: Ticker): F[Option[CompanyProfile]]
  def findAll(limit: Option[Int]): F[List[CompanyProfile]]
  def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int]): F[List[Ticker]]
  def updatePricePerformanceLastUpdated(tickers: List[Ticker]): F[Unit]

final private class LiveCompanyProfileRepository[F[_]](
    private val collection: MongoCollection[F, CompanyProfileEntity]
)(using
    M: Monad[F],
    C: Clock[F]
) extends CompanyProfileRepository[F] {

  private object Field:
    val Id                            = "_id"
    val Name                          = "name"
    val Country                       = "country"
    val Industry                      = "industry"
    val Description                   = "description"
    val Website                       = "website"
    val IpoDate                       = "ipoDate"
    val Currency                      = "currency"
    val MarketCap                     = "marketCap"
    val PricePerformanceLastUpdatedAt = "pricePerformanceLastUpdatedAt"
    val UpdatedAt                     = "updatedAt"
    val CreatedAt                     = "createdAt"

  extension (cp: CompanyProfile)
    private def toUpdate(now: Instant): Update =
      Update
        .setOnInsert(Field.Id, cp.ticker)
        .setOnInsert(Field.CreatedAt, now)
        .set(Field.Name, cp.name)
        .set(Field.Country, cp.country)
        .set(Field.Industry, cp.industry)
        .set(Field.Description, cp.description)
        .set(Field.Website, cp.website)
        .set(Field.IpoDate, cp.ipoDate)
        .set(Field.Currency, cp.currency)
        .set(Field.MarketCap, cp.marketCap)
        .set(Field.UpdatedAt, now)

  override def save(cp: CompanyProfile): F[Unit] =
    C.now.flatMap { time =>
      collection.updateOne(Filter.idEq(cp.ticker), cp.toUpdate(time), UpdateOptions(upsert = true)).void
    }

  override def save(cps: List[CompanyProfile]): F[Unit] =
    C.now.flatMap { time =>
      val updates = cps.map { cp =>
        WriteCommand.UpdateOne(
          Filter.idEq(cp.ticker),
          cp.toUpdate(time),
          UpdateOptions(upsert = true)
        )
      }
      collection.bulkWrite(updates).void
    }

  override def find(ticker: Ticker): F[Option[CompanyProfile]] =
    collection.find(Filter.idEq(ticker)).first.mapOpt(_.toDomain)

  override def findAll(limit: Option[Int]): F[List[CompanyProfile]] =
    collection.find
      .sort(Sort.desc(Field.MarketCap))
      .limit(limit.getOrElse(Int.MaxValue))
      .all
      .mapList(_.toDomain)

  override def findTickersBy(filter: CompanyProfileFilter, limit: Option[Int]): F[List[Ticker]] =
    filter.toFilter.flatMap { mongoFilter =>
      collection
        .aggregate[Entity](
          Aggregate
            .matchBy(mongoFilter)
            .sort(Sort.desc(Field.MarketCap))
            .limit(limit.getOrElse(Int.MaxValue))
            .project(Projection.include(Field.Id))
        )
        .all
        .mapList(_._id)
    }
    
  override def updatePricePerformanceLastUpdated(tickers: List[Ticker]): F[Unit] =
    M.whenA(tickers.nonEmpty) {
      collection
        .updateMany(
          Filter.in(Field.Id, tickers.map(_.value)),
          Update.currentDate(Field.PricePerformanceLastUpdatedAt)
        )
        .void
    }

  extension (f: CompanyProfileFilter)
    private def toFilter: F[Filter] = f match
      case CompanyProfileFilter.MarketCapAbove(min) =>
        Filter.gt(Field.MarketCap, min).pure
      case CompanyProfileFilter.MarketCapBelow(max) =>
        Filter.lt(Field.MarketCap, max).pure
      case CompanyProfileFilter.CountryIs(countryCode) =>
        Filter.eq(Field.Country, countryCode).pure
      case CompanyProfileFilter.IpoDateAfter(date) =>
        Filter.gt(Field.IpoDate, date).pure
      case CompanyProfileFilter.IpoDateBefore(date) =>
        Filter.lt(Field.IpoDate, date).pure
      case CompanyProfileFilter.UpdatedWithin(duration) =>
        C.now.map(currentTime => Filter.gt(Field.UpdatedAt, currentTime.minus(duration)))
      case CompanyProfileFilter.NotUpdatedFor(duration) =>
        C.now.map(currentTime => Filter.lt(Field.UpdatedAt, currentTime.minus(duration)))
      case CompanyProfileFilter.PricePerformanceNotUpdatedFor(duration) =>
        val isNullOrLt = (ts: Instant) => Filter.isNull(Field.PricePerformanceLastUpdatedAt) || Filter.lt(Field.PricePerformanceLastUpdatedAt, ts)
        C.now.map(currentTime => isNullOrLt(currentTime.minus(duration)))
      case CompanyProfileFilter.TickerMatching(pattern) =>
        Filter.regex(Field.Id, pattern).pure
      case CompanyProfileFilter.Composite(filters) =>
        filters.traverse(_.toFilter).map(_.toList.foldLeft(Filter.empty)(_ && _))
}

object CompanyProfileRepository extends MongoJsonCodecs:
  def make[F[_]: {Monad, Clock}](database: MongoDatabase[F]): F[CompanyProfileRepository[F]] =
    database
      .getCollectionWithCodec[CompanyProfileEntity]("company-profiles")
      .map(_.withAddedCodec[Ticker].withAddedCodec[Entity])
      .map(LiveCompanyProfileRepository[F](_))