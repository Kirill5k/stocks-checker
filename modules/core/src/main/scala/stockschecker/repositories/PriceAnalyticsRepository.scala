package stockschecker.repositories

import cats.Monad
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
import mongo4cats.operations.{Filter, Index, Sort, Update}
import stockschecker.domain.{PriceAnalytics, PriceAnalyticsFilter, PricePerformanceSummary, StockAnalysisMetrics, StockAnalysisScores, Ticker}
import stockschecker.repositories.entities.PriceAnalyticsEntity

import java.time.Instant

trait PriceAnalyticsRepository[F[_]]:
  def save(analytics: PriceAnalytics): F[Unit]
  def save(analyticsList: List[PriceAnalytics]): F[Unit]
  def find(ticker: Ticker): F[Option[PriceAnalytics]]
  def findAll(limit: Option[Int]): F[List[PriceAnalytics]]
  def findBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]]

final private class LivePriceAnalyticsRepository[F[_]](
    private val collection: MongoCollection[F, PriceAnalyticsEntity]
)(using
    F: Monad[F],
    clock: Clock[F]
) extends PriceAnalyticsRepository[F] {

  import PriceAnalyticsRepository.Field

  extension (analytics: PriceAnalytics)
    private def toUpdate(now: Instant): Update =
      Update
        .setOnInsert(Field.Id, analytics.ticker.value)
        .setOnInsert(Field.CreatedAt, now)
        .set(Field.UpdatedAt, now)
        .set(Field.PerformanceSummary.Root, analytics.performanceSummary)
        .set(Field.Metrics.Root, analytics.metrics)
        .set(Field.Scores.Root, analytics.scores)

  override def save(analytics: PriceAnalytics): F[Unit] =
    clock.now.flatMap { now =>
      collection.updateOne(Filter.idEq(analytics.ticker.value), analytics.toUpdate(now), UpdateOptions(upsert = true)).void
    }

  override def save(analyticsList: List[PriceAnalytics]): F[Unit] =
    clock.now.flatMap { now =>
      val updateOpt = UpdateOptions(upsert = true)
      val updates   = analyticsList.map { a =>
        WriteCommand.UpdateOne(Filter.idEq(a.ticker.value), a.toUpdate(now), updateOpt)
      }
      collection.bulkWrite(updates).void
    }

  override def find(ticker: Ticker): F[Option[PriceAnalytics]] =
    collection
      .find(Filter.idEq(ticker.value))
      .first
      .mapOpt(_.toDomain)

  override def findAll(limit: Option[Int]): F[List[PriceAnalytics]] =
    collection
      .find(Filter.empty)
      .sort(Sort.desc(Field.Scores.OverallScore))
      .limit(limit.getOrElse(Int.MaxValue))
      .all
      .mapList(_.toDomain)

  override def findBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]] =
    filter.toFilter.flatMap { mongoFilter =>
      collection
        .find(mongoFilter)
        .sort(Sort.desc(Field.Scores.OverallScore))
        .limit(limit.getOrElse(Int.MaxValue))
        .all
        .mapList(_.toDomain)
    }

  extension (f: PriceAnalyticsFilter)
    private def toFilter: F[Filter] = f match
      case PriceAnalyticsFilter.PriceAbove(minPrice) =>
        Filter.gte(Field.PerformanceSummary.LatestPrice, minPrice).pure
      case PriceAnalyticsFilter.PriceBelow(maxPrice) =>
        Filter.lte(Field.PerformanceSummary.LatestPrice, maxPrice).pure
      case PriceAnalyticsFilter.PerformanceAbove(period, minPercentage) =>
        val fieldName = s"${Field.PerformanceSummary.Root}.${period.fieldName}"
        Filter.gte(fieldName, minPercentage).pure
      case PriceAnalyticsFilter.PerformanceBelow(period, maxPercentage) =>
        val fieldName = s"${Field.PerformanceSummary.Root}.${period.fieldName}"
        Filter.lte(fieldName, maxPercentage).pure
      case PriceAnalyticsFilter.UpdatedWithin(duration) =>
        clock.now.map(currentTime => Filter.gt(Field.UpdatedAt, currentTime.minus(duration)))
      case PriceAnalyticsFilter.NotUpdatedFor(duration) =>
        clock.now.map(currentTime => Filter.lt(Field.UpdatedAt, currentTime.minus(duration)))
      case PriceAnalyticsFilter.Composite(filters) =>
        filters.traverse(_.toFilter).map(_.toList.foldLeft(Filter.empty)(_ && _))
}

object PriceAnalyticsRepository extends MongoJsonCodecs:
  val CollectionName = "price-analytics"

  object Field:
    val Id        = "_id"
    val CreatedAt = "createdAt"
    val UpdatedAt = "updatedAt"

    object PerformanceSummary:
      val Root        = "performanceSummary"
      val LatestPrice = "performanceSummary.latestPrice"

    object Metrics:
      val Root = "metrics"

    object Scores:
      val Root             = "scores"
      val OverallScore     = "scores.overallScore"
      val CagrScore        = "scores.cagrScore"
      val VolatilityScore  = "scores.volatilityScore"
      val DrawdownScore    = "scores.drawdownScore"
      val ConsistencyScore = "scores.consistencyScore"

  def make[F[_]: {Concurrent, Clock}](database: MongoDatabase[F]): F[PriceAnalyticsRepository[F]] =
    for
      collection <- database.getCollectionWithCodec[PriceAnalyticsEntity](CollectionName)
      _          <- collection.createIndex(Index.descending(Field.Scores.OverallScore))
      _          <- collection.createIndex(Index.ascending(Field.UpdatedAt))
      collWithCodecs = collection
        .withAddedCodec[Ticker]
        .withAddedCodec[PricePerformanceSummary]
        .withAddedCodec[StockAnalysisMetrics]
        .withAddedCodec[StockAnalysisScores]
    yield LivePriceAnalyticsRepository[F](collWithCodecs)
