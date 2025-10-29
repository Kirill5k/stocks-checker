package stockschecker.repositories

import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UpdateOptions, WriteCommand}
import mongo4cats.operations.{Filter, Update}
import stockschecker.domain.{PricePerformanceSummary, Ticker}
import stockschecker.repositories.entities.PricePerformanceSummaryEntity

import java.time.Instant

trait PricePerformanceSummaryRepository[F[_]]:
  def save(summary: PricePerformanceSummary): F[Unit]
  def find(ticker: Ticker): F[Option[PricePerformanceSummary]]

final private class LivePricePerformanceSummaryRepository[F[_]](
    private val collection: MongoCollection[F, PricePerformanceSummaryEntity]
)(using
    F: Concurrent[F],
    clock: Clock[F]
) extends PricePerformanceSummaryRepository[F] {

  private object Field:
    val Id                = "_id"
    val Ticker            = "ticker"
    val LatestPrice       = "latestPrice"
    val LatestPriceDate   = "latestPriceDate"
    val OneMonthChange    = "oneMonthChange"
    val ThreeMonthChange  = "threeMonthChange"
    val SixMonthChange    = "sixMonthChange"
    val OneYearChange     = "oneYearChange"
    val ThreeYearChange   = "threeYearChange"
    val FiveYearChange    = "fiveYearChange"
    val TenYearChange     = "tenYearChange"
    val MaxChange         = "maxChange"
    val CreatedAt         = "createdAt"
    val UpdatedAt         = "updatedAt"

  extension (summary: PricePerformanceSummary)
    private def toUpdateCommand(now: Instant): WriteCommand[Nothing] =
      val id = summary.ticker.value
      WriteCommand.UpdateOne(
        Filter.idEq(id),
        Update
          .setOnInsert(Field.Id, id)
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
          .set(Field.MaxChange, summary.maxChange),
        UpdateOptions(upsert = true)
      )

  override def save(summary: PricePerformanceSummary): F[Unit] =
    clock.now.flatMap { now =>
      collection.bulkWrite(List(summary.toUpdateCommand(now))).void
    }

  override def find(ticker: Ticker): F[Option[PricePerformanceSummary]] =
    collection.find(Filter.idEq(ticker.value)).first.mapOpt(_.toDomain)
}

object PricePerformanceSummaryRepository extends MongoJsonCodecs:
  def make[F[_]](database: MongoDatabase[F])(using Concurrent[F], Clock[F]): F[PricePerformanceSummaryRepository[F]] =
    database
      .getCollectionWithCodec[PricePerformanceSummaryEntity]("price-performance-summaries")
      .map(_.withAddedCodec[Ticker])
      .map(LivePricePerformanceSummaryRepository[F](_))


