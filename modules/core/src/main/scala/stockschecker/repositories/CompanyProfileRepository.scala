package stockschecker.repositories

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import fs2.Stream
import kirill5k.common.cats.Clock
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Update}
import mongo4cats.circe.MongoJsonCodecs
import stockschecker.domain.{CompanyProfile, CompanyProfileFilter, Ticker}
import stockschecker.repositories.entities.CompanyProfileEntity
import kirill5k.common.cats.syntax.applicative.*

trait CompanyProfileRepository[F[_]]:
  def save(cp: CompanyProfile): F[Unit]
  def find(ticker: Ticker): F[Option[CompanyProfile]]
  def streamTickersBy(filter: CompanyProfileFilter, limit: Option[Int]): Stream[F, Ticker]

final private class LiveCompanyProfileRepository[F[_]: Monad](
    private val collection: MongoCollection[F, CompanyProfileEntity]
)(using
    C: Clock[F]
) extends CompanyProfileRepository[F] {

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
                  .set("name", cp.name)
                  .set("country", cp.country)
                  .set("industry", cp.industry)
                  .set("description", cp.description)
                  .set("website", cp.website)
                  .set("ipoDate", cp.ipoDate)
                  .set("currency", cp.currency)
                  .set("marketCap", cp.marketCap)
                  .currentDate("updatedAt")
              )
              .void
        }
    }

  override def find(ticker: Ticker): F[Option[CompanyProfile]] =
    collection.find(Filter.idEq(ticker)).first.mapOpt(_.toDomain)

  override def streamTickersBy(filter: CompanyProfileFilter, limit: Option[Int]): Stream[F, Ticker] =
    collection.find(filter.toFilter).sortByDesc("marketCap").limit(limit.getOrElse(Int.MaxValue)).stream.map(_._id)

  extension (f: CompanyProfileFilter)
    private def toFilter: Filter = f match
      case CompanyProfileFilter.MarketCapAbove(min) =>
        Filter.gt("marketCap", min)
      case CompanyProfileFilter.MarketCapBelow(max) =>
        Filter.lt("marketCap", max)
      case CompanyProfileFilter.CountryIs(countryCode) =>
        Filter.eq("country", countryCode)
      case CompanyProfileFilter.IpoDateAfter(date) =>
        Filter.gt("ipoDate", date)
      case CompanyProfileFilter.IpoDateBefore(date) =>
        Filter.lt("ipoDate", date)
      case CompanyProfileFilter.UpdatedAfter(date) =>
        Filter.gt("updatedAt", date)
      case CompanyProfileFilter.UpdatedBefore(date) =>
        Filter.lt("updatedAt", date)
      case CompanyProfileFilter.Composite(filters) =>
        filters.map(_.toFilter).foldLeft(Filter.empty)(_ && _)
}

object CompanyProfileRepository extends MongoJsonCodecs:
  def make[F[_]: {Monad, Clock}](database: MongoDatabase[F]): F[CompanyProfileRepository[F]] =
    database
      .getCollectionWithCodec[CompanyProfileEntity]("company-profiles")
      .map(_.withAddedCodec[Ticker])
      .map(LiveCompanyProfileRepository[F](_))
