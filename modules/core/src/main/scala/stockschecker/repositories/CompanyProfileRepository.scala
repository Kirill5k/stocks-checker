package stockschecker.repositories

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.repositories.entities.CompanyProfileEntity
import kirill5k.common.cats.syntax.applicative.*

trait CompanyProfileRepository[F[_]]:
  def save(ticker: Ticker, cp: CompanyProfile): F[Unit]
  def find(ticker: Ticker): F[Option[CompanyProfile]]

final private class LiveCompanyProfileRepository[F[_]: {Monad, Clock}](
    private val collection: MongoCollection[F, CompanyProfileEntity]
) extends CompanyProfileRepository[F] {

  override def save(ticker: Ticker, cp: CompanyProfile): F[Unit] =
    Clock[F].now.flatMap { time =>
      collection
        .count(Filter.idEq(ticker))
        .flatMap {
          case 0 =>
            collection.insertOne(CompanyProfileEntity.from(cp, ticker, time)).void
          case _ =>
            collection
              .updateOne(
                Filter.idEq(ticker),
                Update
                  .set("name", cp.name)
                  .set("country", cp.country)
                  .set("sector", cp.sector)
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
}

object CompanyProfileRepository:
  def make[F[_]: {Monad, Clock}](database: MongoDatabase[F]): F[CompanyProfileRepository[F]] =
    database
      .getCollectionWithCodec[CompanyProfileEntity]("company-profiles")
      .map(LiveCompanyProfileRepository[F](_))
