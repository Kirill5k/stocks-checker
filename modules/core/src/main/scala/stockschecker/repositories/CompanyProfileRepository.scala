package stockschecker.repositories

import cats.Monad
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.repositories.entities.CompanyProfileEntity
import kirill5k.common.cats.syntax.applicative.*

trait CompanyProfileRepository[F[_]]:
  def save(cp: CompanyProfile): F[Unit]
  def find(ticker: Ticker): F[Option[CompanyProfile]]

final private class LiveCompanyProfileRepository[F[_]: Monad](
    private val collection: MongoCollection[F, CompanyProfileEntity]
) extends CompanyProfileRepository[F] {

  override def save(cp: CompanyProfile): F[Unit] =
    collection
      .count(Filter.idEq(cp.ticker))
      .flatMap {
        case 0 =>
          collection.insertOne(CompanyProfileEntity.from(cp)).void
        case _ =>
          collection
            .updateOne(
              Filter.idEq(cp.ticker),
              Update
                .set("averageTradedVolume", cp.averageTradedVolume)
                .set("marketCap", cp.marketCap)
                .currentDate("lastUpdatedAt")
            )
            .void
      }

  override def find(ticker: Ticker): F[Option[CompanyProfile]] =
    collection.find(Filter.idEq(ticker)).first.mapOpt(_.toDomain)
}

object CompanyProfileRepository:
  def make[F[_]: Monad](database: MongoDatabase[F]): F[CompanyProfileRepository[F]] =
    database
      .getCollectionWithCodec[CompanyProfileEntity]("company-profiles")
      .map(LiveCompanyProfileRepository[F](_))
