package stockschecker.repositories

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.functor.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Sort}
import stockschecker.domain.{Stock, Ticker}
import stockschecker.repositories.entities.StockEntity

trait StockRepository[F[_]]:
  def find(ticker: Ticker): F[Option[Stock]]
  def findAll(limit: Option[Int]): F[List[Stock]]
  def findByTickers(tickers: NonEmptyList[Ticker]): F[List[Stock]]

final private class LiveStockRepository[F[_]](
    private val collection: MongoCollection[F, StockEntity]
)(using
    F: Monad[F]
) extends StockRepository[F] {
  import StockRepository.Field

  private def buildAggregation(filter: Filter, limit: Int): Aggregate =
    Aggregate
      .matchBy(filter)
      .replaceWith(Document("security" := "$$ROOT"))
      .lookup(CompanyProfileRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.Profile)
      .lookup(PricePerformanceSummaryRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.PerformanceSummary)
      .sort(Sort.desc(s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"))
      .limit(limit)

  override def find(ticker: Ticker): F[Option[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(Filter.idEq(ticker.value), 1))
      .first
      .mapOpt(_.toDomain)

  override def findAll(limit: Option[Int]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(Filter.empty, limit.getOrElse(Int.MaxValue)))
      .all
      .mapList(_.toDomain)

  override def findByTickers(tickers: NonEmptyList[Ticker]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(Filter.in(Field.Id, tickers.toList.map(_.value)), tickers.size))
      .all
      .mapList(_.toDomain)
}

object StockRepository extends MongoJsonCodecs:
  object Field:
    val Id                 = "_id"
    val Profile            = "profile"
    val PerformanceSummary = "performanceSummary"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity](SecurityRepository.CollectionName)
      .map(LiveStockRepository[F](_))
