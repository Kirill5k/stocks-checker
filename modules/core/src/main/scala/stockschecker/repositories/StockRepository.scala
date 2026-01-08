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
import stockschecker.common.types.EnumType
import stockschecker.domain.{Exchange, SecurityKind, Stock, Ticker, TimePeriod}
import stockschecker.repositories.entities.StockEntity

object StockSortField extends EnumType[StockSortField](() => StockSortField.values)
enum StockSortField:
  case MarketCap
  case OverallScore
  case CagrScore
  case VolatilityScore

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
    period: Option[TimePeriod] = None,
    minOverallScore: Option[BigDecimal] = None,
    minCagrScore: Option[BigDecimal] = None,
    minVolatilityScore: Option[BigDecimal] = None,
    sortBy: Option[StockSortField] = None
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
    val sortField = filters.sortBy match
      case Some(StockSortField.MarketCap)       => s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"
      case Some(StockSortField.OverallScore)    => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.OverallScore}"
      case Some(StockSortField.CagrScore)       => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.CagrScore}"
      case Some(StockSortField.VolatilityScore) => s"${Field.PriceAnalytics}.${PriceAnalyticsRepository.Field.Scores.VolatilityScore}"
      case None                                 => s"${Field.Profile}.${CompanyProfileRepository.Field.MarketCap}"

    Aggregate
      .matchBy(securityFilter)
      .replaceWith(Document("security" := "$$ROOT"))
      .lookup(CompanyProfileRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.Profile)
      .lookup(PriceAnalyticsRepository.CollectionName, s"security.${Field.Id}", Field.Id, Field.PriceAnalytics)
      .matchBy(filters.toProfileFilter && filters.toPriceAnalyticsFilter)
      .sort(Sort.desc(sortField))
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

    private def toPriceAnalyticsFilter: Filter = List(
      sf.minPrice.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.LatestPrice}", min)
      ),
      sf.maxPrice.map(max =>
        Filter.lte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.LatestPrice}", max)
      ),
      (sf.minChange, sf.period).mapN { (min, period) =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.Root}.${period.fieldName}", min)
      },
      (sf.maxChange, sf.period).mapN { (max, period) =>
        Filter.lte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.PerformanceSummary.Root}.${period.fieldName}", max)
      },
      sf.minOverallScore.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.OverallScore}", min)
      ),
      sf.minCagrScore.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.CagrScore}", min)
      ),
      sf.minVolatilityScore.map(min =>
        Filter.gte(s"${StockRepository.Field.PriceAnalytics}.0.${PriceAnalyticsRepository.Field.Scores.VolatilityScore}", min)
      )
    ).flatten.foldLeft(Filter.empty)(_ && _)
}

object StockRepository extends MongoJsonCodecs:
  object Field:
    val Id             = "_id"
    val Profile        = "profile"
    val PriceAnalytics = "priceAnalytics"

  def make[F[_]: Concurrent](database: MongoDatabase[F]): F[StockRepository[F]] =
    database
      .getCollectionWithCodec[StockEntity](SecurityRepository.CollectionName)
      .map(_.withAddedCodec[Exchange].withAddedCodec[SecurityKind])
      .map(LiveStockRepository[F](_))
