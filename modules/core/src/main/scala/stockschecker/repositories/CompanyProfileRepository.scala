package stockschecker.repositories

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import fs2.Stream
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Projection, Sort, Update}
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.entities.{CompanyProfileEntity, Entity}
import kirill5k.common.cats.syntax.applicative.*

trait CompanyProfileRepository[F[_]]:
  def save(cp: CompanyProfile): F[Unit]
  def find(ticker: Ticker): F[Option[CompanyProfile]]
  def findAll(limit: Option[Int]): F[List[CompanyProfile]]
  def streamTickersBy(filter: CompanyProfileFilter, limit: Option[Int]): Stream[F, Ticker]

final private class LiveCompanyProfileRepository[F[_]](
    private val collection: MongoCollection[F, CompanyProfileEntity]
)(using
    M: Monad[F],
    C: Clock[F]
) extends CompanyProfileRepository[F] {

  private object Field:
    val Id          = "_id"
    val Name        = "name"
    val Country     = "country"
    val Industry    = "industry"
    val Description = "description"
    val Website     = "website"
    val IpoDate     = "ipoDate"
    val Currency    = "currency"
    val MarketCap   = "marketCap"
    val UpdatedAt   = "updatedAt"
    val CreatedAt   = "createdAt"

  override def save(cp: CompanyProfile): F[Unit] =
    C.now.flatMap { time =>
      collection
        .count(Filter.idEq(cp.ticker))
        .flatMap {
          case 0 =>
            collection.insertOne(CompanyProfileEntity.from(cp, time)).void
          case _ =>
            collection
              .updateOne(
                Filter.idEq(cp.ticker),
                Update
                  .set(Field.Name, cp.name)
                  .set(Field.Country, cp.country)
                  .set(Field.Industry, cp.industry)
                  .set(Field.Description, cp.description)
                  .set(Field.Website, cp.website)
                  .set(Field.IpoDate, cp.ipoDate)
                  .set(Field.Currency, cp.currency)
                  .set(Field.MarketCap, cp.marketCap)
                  .currentDate(Field.UpdatedAt)
              )
              .void
        }
    }

  override def find(ticker: Ticker): F[Option[CompanyProfile]] =
    collection.find(Filter.idEq(ticker)).first.mapOpt(_.toDomain)

  override def findAll(limit: Option[Int]): F[List[CompanyProfile]] =
    collection
      .find
      .sort(Sort.desc(Field.MarketCap))
      .limit(limit.getOrElse(Int.MaxValue))
      .all
      .mapList(_.toDomain)

  override def streamTickersBy(filter: CompanyProfileFilter, limit: Option[Int]): Stream[F, Ticker] =
    Stream.eval(filter.toFilter).flatMap { mongoFilter =>
      collection
        .aggregate[Entity](
          Aggregate
            .matchBy(mongoFilter)
            .sort(Sort.desc(Field.MarketCap))
            .limit(limit.getOrElse(Int.MaxValue))
            .project(Projection.include(Field.Id))
        )
        .stream
        .map(_._id)
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
      case CompanyProfileFilter.Composite(filters) =>
        filters.traverse(_.toFilter).map(_.toList.foldLeft(Filter.empty)(_ && _))
}

object CompanyProfileRepository extends MongoJsonCodecs:
  def make[F[_]: {Monad, Clock}](database: MongoDatabase[F]): F[CompanyProfileRepository[F]] =
    database
      .getCollectionWithCodec[CompanyProfileEntity]("company-profiles")
      .map(_.withAddedCodec[Ticker].withAddedCodec[Entity])
      .map(LiveCompanyProfileRepository[F](_))
