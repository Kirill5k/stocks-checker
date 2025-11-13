package stockschecker.repositories

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import cats.syntax.foldable.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.syntax.applicative.*
import kirill5k.common.syntax.time.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Filter, Sort, Update}
import stockschecker.domain.{PricePerformanceSummary, PricePerformanceFilter, Ticker}
import stockschecker.repositories.entities.PricePerformanceSummaryEntity

import java.time.Instant

trait PricePerformanceSummaryRepository[F[_]]:
  def save(summary: PricePerformanceSummary): F[Unit]
  def save(summaries: List[PricePerformanceSummary]): F[Unit]
  def find(ticker: Ticker): F[Option[PricePerformanceSummary]]
  def findAll(limit: Option[Int]): F[List[PricePerformanceSummary]]
  def findBy(filter: PricePerformanceFilter, limit: Option[Int]): F[List[PricePerformanceSummary]]

final private class LivePricePerformanceSummaryRepository[F[_]](
    private val collection: MongoCollection[F, PricePerformanceSummaryEntity]
)(using
    F: Monad[F],
    clock: Clock[F]
) extends PricePerformanceSummaryRepository[F] {

  private object Field:
    val Id               = "_id"
    val Ticker           = "ticker"
    val LatestPrice      = "latestPrice"
    val LatestPriceDate  = "latestPriceDate"
    val OneMonthChange   = "oneMonthChange"
    val ThreeMonthChange = "threeMonthChange"
    val SixMonthChange   = "sixMonthChange"
    val OneYearChange    = "oneYearChange"
    val ThreeYearChange  = "threeYearChange"
    val FiveYearChange   = "fiveYearChange"
    val TenYearChange    = "tenYearChange"
    val MaxChange        = "maxChange"
    val CreatedAt        = "createdAt"
    val UpdatedAt        = "updatedAt"

  extension (summary: PricePerformanceSummary)
    private def toUpdate(now: Instant): Update =
      Update
        .setOnInsert(Field.Id, summary.ticker)
        .setOnInsert(Field.CreatedAt, now)
        .set(Field.UpdatedAt, now)
        .set(Field.Ticker, summary.ticker)
        .set(Field.LatestPrice, summary.latestPrice)
        .set(Field.LatestPriceDate, summary.latestPriceDate)
        .set(Field.OneMonthChange, summary.oneMonthChange)
        .set(Field.ThreeMonthChange, summary.threeMonthChange)
        .set(Field.SixMonthChange, summary.sixMonthChange)
        .set(Field.OneYearChange, summary.oneYearChange)
        .set(Field.ThreeYearChange, summary.threeYearChange)
        .set(Field.FiveYearChange, summary.fiveYearChange)
        .set(Field.TenYearChange, summary.tenYearChange)
        .set(Field.MaxChange, summary.maxChange)

  override def save(summary: PricePerformanceSummary): F[Unit] =
    clock.now.flatMap { now =>
      collection.updateOne(Filter.idEq(summary.ticker), summary.toUpdate(now), UpdateOptions(upsert = true)).void
    }

  override def save(summaries: List[PricePerformanceSummary]): F[Unit] =
    clock.now.flatMap { now =>
      val updates = summaries.map { s =>
        WriteCommand.UpdateOne(
          Filter.idEq(s.ticker),
          s.toUpdate(now),
          UpdateOptions(upsert = true)
        )
      }
      collection.bulkWrite(updates).void
    }

  override def find(ticker: Ticker): F[Option[PricePerformanceSummary]] =
    collection.find(Filter.idEq(ticker.value)).first.mapOpt(_.toDomain)

  override def findAll(limit: Option[Int]): F[List[PricePerformanceSummary]] =
    collection.find
      .sort(Sort.desc(Field.OneYearChange))
      .limit(limit.getOrElse(Int.MaxValue))
      .all
      .mapList(_.toDomain)

  override def findBy(filter: stockschecker.domain.PricePerformanceFilter, limit: Option[Int]): F[List[PricePerformanceSummary]] =
    filter.toFilter.flatMap { mongoFilter =>
      collection.find
        .filter(mongoFilter)
        .sort(Sort.desc(Field.OneYearChange))
        .limit(limit.getOrElse(Int.MaxValue))
        .all
        .mapList(_.toDomain)
    }

  extension (f: PricePerformanceFilter)
    private def toFilter: F[Filter] = f match
      case PricePerformanceFilter.PriceAbove(minPrice) =>
        Filter.gte(Field.LatestPrice, minPrice).pure
      case PricePerformanceFilter.PriceBelow(maxPrice) =>
        Filter.lte(Field.LatestPrice, maxPrice).pure
      case PricePerformanceFilter.PerformanceAbove(period, minPercentage) =>
        Filter.gte(period.fieldName, minPercentage).pure
      case PricePerformanceFilter.PerformanceBelow(period, maxPercentage) =>
        Filter.lte(period.fieldName, maxPercentage).pure
      case PricePerformanceFilter.UpdatedWithin(duration) =>
        clock.now.map(currentTime => Filter.gte(Field.UpdatedAt, currentTime.minus(duration)))
      case PricePerformanceFilter.NotUpdatedFor(duration) =>
        clock.now.map(currentTime => Filter.lte(Field.UpdatedAt, currentTime.minus(duration)))
      case PricePerformanceFilter.Composite(filters) =>
        filters.toList.foldLeftM(Filter.empty) { (acc, filter) =>
          filter.toFilter.map(f => acc && f)
        }
}

object PricePerformanceSummaryRepository extends MongoJsonCodecs:
  val CollectionName = "price-performance-summaries"
  
  def make[F[_]: {Concurrent, Clock}](database: MongoDatabase[F]): F[PricePerformanceSummaryRepository[F]] =
    database
      .getCollectionWithCodec[PricePerformanceSummaryEntity](CollectionName)
      .map(_.withAddedCodec[Ticker])
      .map(LivePricePerformanceSummaryRepository[F](_))
