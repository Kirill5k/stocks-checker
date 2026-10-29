package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.bson.{BsonValue, Document}
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{UnwindOptions, UpdateOptions}
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.*
import stockschecker.domain.investment.EvaluationCapture
import stockschecker.repositories.entities.{
  EvaluationPriceSeriesEntity,
  InvestmentEvaluationContextEntity,
  InvestmentEvaluationEntity,
  InvestmentEvaluationTaskEntity,
  InvestmentScreenEntity,
  PriceHistoryEntity
}

import java.time.YearMonth

trait InvestmentEvaluationRepository[F[_]]:
  def saveMonthly(evaluation: InvestmentEvaluation): F[Unit]
  def recordHistoryRequests(screenId: InvestmentScreenId, month: YearMonth, tickers: List[Ticker]): F[Unit]
  def findContext(screenId: InvestmentScreenId, month: YearMonth): F[Option[EvaluationCapture.Context]]
  def captureTasks(month: YearMonth): F[List[EvaluationCapture.Task]]
  def findPriceSeries(tickers: List[Ticker], range: DateRange): F[Map[Ticker, EvaluationPriceSeries]]

final private class LiveInvestmentEvaluationRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentEvaluationEntity],
    private val screens: MongoCollection[F, InvestmentScreenEntity],
    private val prices: MongoCollection[F, PriceHistoryEntity]
)(using F: MonadThrow[F])
    extends InvestmentEvaluationRepository[F] {
  import InvestmentEvaluationRepository.{CollectionName, Field}

  override def saveMonthly(evaluation: InvestmentEvaluation): F[Unit] =
    val month    = evaluation.captureMonth
    val id       = s"${evaluation.screenId.value}:$month"
    val writable = Filter.idEq(id) && Filter.ne(Field.Settled, true)
    val update   = Update
      .setOnInsert(Field.RequestedHistory, List.empty[String])
      .set(Field.ScreenId, evaluation.screenId.value)
      .set(Field.Month, month.toString)
      .set(Field.Evaluation, evaluation)
      .set(Field.Complete, evaluation.isComplete)
      .set(Field.Settled, evaluation.settled)
      .set(Field.Finished, evaluation.isFinished)
    collection.updateOne(writable, update, UpdateOptions(upsert = true)).void.handleErrorWith {
      // A settled winner remains immutable when another capture inserts first or settles between writes.
      case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 =>
        collection.updateOne(writable, update).void
      case error => F.raiseError(error)
    }

  override def recordHistoryRequests(screenId: InvestmentScreenId, month: YearMonth, tickers: List[Ticker]): F[Unit] =
    F.whenA(tickers.nonEmpty)(
      collection
        .updateOne(
          Filter.idEq(s"${screenId.value}:$month"),
          Update.addEachToSet(Field.RequestedHistory, tickers.map(_.value))
        )
        .void
    )

  override def findContext(screenId: InvestmentScreenId, month: YearMonth): F[Option[EvaluationCapture.Context]] =
    screens
      .aggregate[InvestmentEvaluationContextEntity](contextAggregation(screenId, month))
      .first
      .mapOpt(_.toDomain)

  override def captureTasks(month: YearMonth): F[List[EvaluationCapture.Task]] =
    screens
      .aggregate[InvestmentEvaluationTaskEntity](taskContextAggregation(month).combinedWith(taskAggregation(month)))
      .all
      .mapList(_.toDomain)

  override def findPriceSeries(tickers: List[Ticker], range: DateRange): F[Map[Ticker, EvaluationPriceSeries]] =
    val price    = PriceHistoryRepository.Field
    val matching = Filter.in("_id", tickers.distinct.map(ticker => s"${ticker.value}:${PriceAdjustment.SplitsAndDividends.value}")) &&
      Filter.lte(price.RangeFrom, range.from.toString) && Filter.gte(price.RangeThrough, range.through.toString) &&
      Filter.gte(price.FetchedAt, PriceHistorySnapshot.completedSessionCutoff(range.through))
    val trimmed = Document(
      "$filter" := Document(
        "input" := "$data.candles",
        "as"    := "candle",
        "cond"  := Document(
          "$and" := List(
            Document("$gte" := List("$$candle.date", range.from.toString)),
            Document("$lte" := List("$$candle.date", range.through.toString))
          )
        )
      )
    )
    val projection = Projection.include("data").computed("candleCount", Document("$size" := "$data.candles")).excludeId
    if tickers.isEmpty then F.pure(Map.empty)
    else
      prices
        .aggregate[EvaluationPriceSeriesEntity](
          Aggregate
            .matchBy(matching)
            .project(projection)
            .set("data.candles" -> trimmed)
        )
        .all
        .map(_.iterator.map(row => row.data.ticker -> row.toDomain(range)).toMap)

  private def expression(operator: String, arguments: BsonValue*): Document = Document(operator := arguments.toList)

  private def contextAggregation(screenId: InvestmentScreenId, month: YearMonth): Aggregate =
    val saved    = Projection.include(Field.Evaluation).excludeId
    val captures = Aggregate
      .matchBy(Filter.eq(Field.ScreenId, screenId.value))
      .facet(
        Aggregate.Facet("monthly", Aggregate.matchBy(Filter.eq(Field.Month, month.toString)).project(saved)),
        Aggregate.Facet("finished", Aggregate.matchBy(Filter.eq(Field.Finished, true)).sort(Sort.desc(Field.Month)).limit(1).project(saved))
      )
    Aggregate
      .matchBy(Filter.idEq(screenId.value))
      .replaceWith(Document("screen" := "$$ROOT"))
      .lookup(CollectionName, captures, "captures")
      .unwind("$captures")
      .project(
        Projection
          .include("screen")
          .computed("monthly", "$captures.monthly.evaluation")
          .computed("finished", "$captures.finished.evaluation")
          .excludeId
      )

  private def selectedCaptures(field: String, expected: BsonValue): Document =
    Document(
      "$filter" := Document(
        "input" := "$captures",
        "as"    := "capture",
        "cond"  := expression("$eq", s"$$$$capture.$field".toBson, expected)
      )
    )

  private def taskContextAggregation(month: YearMonth): Aggregate =
    // Immediate unwind is coalesced into the lookup, so full historical reports never form one large array.
    // Only compact request state is regrouped; even a screen's combined reports can exceed Mongo's document limit.
    Aggregate
      .replaceWith(Document("screen" := "$$ROOT"))
      .lookup(CollectionName, "screen._id", Field.ScreenId, "captures")
      .unwind("$captures", UnwindOptions(preserveNullAndEmptyArrays = true))
      .project(
        Projection
          .include("screen")
          .computed(
            "capture",
            Document(
              Field.Month            := "$captures.month",
              Field.Settled          := "$captures.settled",
              Field.Finished         := "$captures.finished",
              Field.RequestedHistory := "$captures.requestedHistory"
            )
          )
          .excludeId
      )
      .group("$screen._id", Accumulator.first("screen", "$screen").push("captures", "$capture"))
      .project(
        Projection
          .include("screen")
          .computed("monthly", selectedCaptures(Field.Month, month.toString.toBson))
          .computed("finished", selectedCaptures(Field.Finished, true.toBson))
          .computed("unsettled", selectedCaptures(Field.Settled, false.toBson))
          .excludeId
      )

  private def taskAggregation(month: YearMonth): Aggregate =
    val settled = Document(
      "$filter" := Document(
        "input" := "$monthly",
        "as"    := "capture",
        "cond"  := "$$capture.settled"
      )
    )
    val needsCurrent = Document(
      "$and" := List(
        expression("$eq", Document("$size" := "$finished").toBson, 0.toBson),
        expression("$eq", Document("$size" := settled).toBson, 0.toBson)
      )
    )
    val current = expression(
      "$cond",
      needsCurrent.toBson,
      List(Document("month" := month.toString, "existing" := "$monthly")).toBson,
      List.empty[Document].toBson
    )
    val unfinished = Document(
      "$map" := Document(
        "input" := "$unsettled",
        "as"    := "capture",
        "in"    := Document("month" := "$$capture.month", "existing" := List("$$capture"))
      )
    )
    Aggregate
      .replaceWith(Document("screen" := "$screen", "tasks" := expression("$setUnion", current.toBson, unfinished.toBson)))
      .unwind("$tasks")
      .replaceWith(Document("screen" := "$screen", "month" := "$tasks.month", "existing" := "$tasks.existing"))
      .sort(Sort.asc("screen.createdAt").asc("month"))
}

object InvestmentEvaluationRepository extends MongoJsonCodecs:
  val CollectionName = "investment-evaluations"

  object Field:
    val ScreenId         = "screenId"
    val Month            = "month"
    val Evaluation       = "evaluation"
    val Complete         = "complete"
    val Settled          = "settled"
    val Finished         = "finished"
    val RequestedHistory = "requestedHistory"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentEvaluationRepository[F]] =
    for
      raw <- db.getCollectionWithCodec[InvestmentEvaluationEntity](CollectionName)
      collection = raw.withAddedCodec[InvestmentEvaluation]
      screens <- db.getCollectionWithCodec[InvestmentScreenEntity](InvestmentScreenRepository.CollectionName)
      prices  <- db.getCollectionWithCodec[PriceHistoryEntity](PriceHistoryRepository.CollectionName)
      _       <- collection.createIndex(Index.ascending(List(Field.ScreenId, Field.Month)))
    yield LiveInvestmentEvaluationRepository[F](
      collection,
      screens.withAddedCodec[InvestmentEvaluationContextEntity].withAddedCodec[InvestmentEvaluationTaskEntity],
      prices.withAddedCodec[EvaluationPriceSeriesEntity]
    )
