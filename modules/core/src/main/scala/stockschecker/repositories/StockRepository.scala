package stockschecker.repositories

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.apply.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Sort}
import stockschecker.domain.{Exchange, SecurityKind, Stock, Ticker, TimePeriod}
import stockschecker.repositories.entities.StockEntity

final case class StockFilters(
    exchange: Option[Exchange] = None,
    kind: Option[SecurityKind] = None,
    country: Option[String] = None,
    minMarketCap: Option[Long] = None,
    maxMarketCap: Option[Long] = None,
    minPrice: Option[BigDecimal] = None,
    maxPrice: Option[BigDecimal] = None,
    minChange: Option[BigDecimal] = None,
    maxChange: Option[BigDecimal] = None,
    period: Option[TimePeriod] = None
)

trait StockRepository[F[_]]:
  def find(ticker: Ticker): F[Option[Stock]]
  def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]]
  def findByTickers(tickers: NonEmptyList[Ticker]): F[List[Stock]]

final private class LiveStockRepository[F[_]](
    private val collection: MongoCollection[F, StockEntity]
)(using
    F: Monad[F]
) extends StockRepository[F] {
  import StockRepository.Field

  private def buildAggregation(securityFilter: Filter, filters: StockFilters, limit: Int): Aggregate =
    Aggregate
      .matchBy(securityFilter)
      .replaceWith(Document("security" := "$$ROOT"))
      .lookup(CompanyProfileRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.Profile)
      .lookup(PricePerformanceSummaryRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.PerformanceSummary)
      .matchBy(filters.toProfileFilter && filters.toPerformanceFilter)
      .sort(Sort.desc(s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"))
      .limit(limit)

  override def find(ticker: Ticker): F[Option[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(Filter.idEq(ticker.value), StockFilters(), 1))
      .first
      .mapOpt(_.toDomain)

  override def findAll(filters: StockFilters, limit: Option[Int]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(filters.toSecurityFilter, filters, limit.getOrElse(Int.MaxValue)))
      .all
      .mapList(_.toDomain)

  override def findByTickers(tickers: NonEmptyList[Ticker]): F[List[Stock]] =
    collection
      .aggregate[StockEntity](buildAggregation(Filter.in(Field.Id, tickers.toList.map(_.value)), StockFilters(), tickers.size))
      .all
      .mapList(_.toDomain)

  extension (sf: StockFilters)
    private def toSecurityFilter: Filter = List(
      sf.exchange.map(e => Filter.eq(SecurityRepository.Field.Exchange, e)),
      sf.kind.map(k => Filter.eq(SecurityRepository.Field.Kind, k))
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toProfileFilter: Filter = List(
      sf.country.map(c => Filter.eq(s"${StockRepository.Field.Profile}.0.${CompanyProfileRepository.Field.Country}", c)),
      sf.minMarketCap.map(min => Filter.gte(s"${StockRepository.Field.Profile}.0.${CompanyProfileRepository.Field.MarketCap}", min)),
      sf.maxMarketCap.map(max => Filter.lte(s"${StockRepository.Field.Profile}.0.${CompanyProfileRepository.Field.MarketCap}", max))
    ).flatten.foldLeft(Filter.empty)(_ && _)

    private def toPerformanceFilter: Filter = List(
      sf.minPrice.map(min =>
        Filter.gte(s"${StockRepository.Field.PerformanceSummary}.0.${PricePerformanceSummaryRepository.Field.LatestPrice}", min)
      ),
      sf.maxPrice.map(max =>
        Filter.lte(s"${StockRepository.Field.PerformanceSummary}.0.${PricePerformanceSummaryRepository.Field.LatestPrice}", max)
      ),
      (sf.minChange, sf.period).mapN { (min, period) =>
        Filter.gte(s"${StockRepository.Field.PerformanceSummary}.0.${period.fieldName}", min)
      },
      (sf.maxChange, sf.period).mapN { (max, period) =>
        Filter.lte(s"${StockRepository.Field.PerformanceSummary}.0.${period.fieldName}", max)
      }
    ).flatten.foldLeft(Filter.empty)(_ && _)
}

object StockRepository extends MongoJsonCodecs:
  object Field:
    val Id                 = "_id"
    val Profile            = "profile"
    val PerformanceSummary = "performanceSummary"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity](SecurityRepository.CollectionName)
      .map(_.withAddedCodec[Exchange].withAddedCodec[SecurityKind])
      .map(LiveStockRepository[F](_))
