package stockschecker.repositories

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.functor.*
import io.circe.Codec
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter}
import stockschecker.domain.{Exchange, SecurityKind, Stock, Ticker}
import stockschecker.repositories.entities.{CompanyProfileEntity, PricePerformanceSummaryEntity, SecurityEntity}

trait StockRepository[F[_]]:
  def find(ticker: Ticker): F[Option[Stock]]
  def findAll(limit: Option[Int]): F[List[Stock]]
  def findByTickers(tickers: List[Ticker]): F[List[Stock]]

final private class LiveStockRepository[F[_]](
    private val collection: MongoCollection[F, SecurityEntity]
)(using
    F: Monad[F]
) extends StockRepository[F] {

  private object Field:
    val id          = "_id"
    val profile     = "profile"
    val performance = "performance"

  final private case class StockEntity(
      security: SecurityEntity,
      profile: List[CompanyProfileEntity],
      performance: List[PricePerformanceSummaryEntity]
  ) derives Codec.AsObject:
    def toDomain: Stock =
      Stock(
        security = security.toDomain,
        profile = profile.headOption.map(_.toDomain),
        performanceSummary = performance.headOption.map(_.toDomain)
      )

  private def buildAggregation(filter: Filter, limit: Int): Aggregate =
    Aggregate
      .matchBy(filter)
      .lookup(CompanyProfileRepository.CollectionName, Field.id, Field.id, Field.profile)
      .lookup(PricePerformanceSummaryRepository.CollectionName, Field.id, Field.id, Field.performance)
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

  override def findByTickers(tickers: List[Ticker]): F[List[Stock]] =
    if tickers.isEmpty then F.pure(List.empty)
    else
      collection
        .aggregate[StockEntity](
          buildAggregation(Filter.in("_id", tickers.map(_.value)), tickers.size)
        )
        .all
        .mapList(_.toDomain)
}

object StockRepository extends MongoJsonCodecs:
  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[SecurityEntity](SecurityRepository.CollectionName)
      .map(_.withAddedCodec[Ticker].withAddedCodec[Exchange].withAddedCodec[SecurityKind])
      .map(LiveStockRepository[F](_))
