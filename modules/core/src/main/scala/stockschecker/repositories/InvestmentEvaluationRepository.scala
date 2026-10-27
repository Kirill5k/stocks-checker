package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.UpdateOptions
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.*
import stockschecker.repositories.entities.{InvestmentEvaluationEntity, InvestmentEvaluationScreenReference}

import java.time.YearMonth

trait InvestmentEvaluationRepository[F[_]]:
  def saveMonthly(evaluation: InvestmentEvaluation): F[Unit]
  def requestedHistory(screenId: InvestmentScreenId, month: YearMonth): F[Set[Ticker]]
  def recordHistoryRequest(screenId: InvestmentScreenId, month: YearMonth, ticker: Ticker): F[Unit]
  def findMonthly(screenId: InvestmentScreenId, month: YearMonth): F[Option[InvestmentEvaluation]]
  def unsettled: F[List[InvestmentEvaluation]]
  def finishedScreenIds: F[Set[InvestmentScreenId]]
  def findFinished(screenId: InvestmentScreenId): F[Option[InvestmentEvaluation]]

final private class LiveInvestmentEvaluationRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentEvaluationEntity]
)(using
    F: MonadThrow[F]
) extends InvestmentEvaluationRepository[F] {
  import InvestmentEvaluationRepository.Field

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
      // Another capture can insert first or settle between writes. Update an existing unfinished
      // document without upsert; a settled winner remains immutable.
      case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 =>
        collection.updateOne(writable, update).void
      case error => F.raiseError(error)
    }

  override def requestedHistory(screenId: InvestmentScreenId, month: YearMonth): F[Set[Ticker]] =
    collection.find(Filter.idEq(s"${screenId.value}:$month")).first.map(_.fold(Set.empty[Ticker])(_.requestedHistory.toSet))

  override def recordHistoryRequest(screenId: InvestmentScreenId, month: YearMonth, ticker: Ticker): F[Unit] =
    collection.updateOne(Filter.idEq(s"${screenId.value}:$month"), Update.addToSet(Field.RequestedHistory, ticker.value)).void

  override def findMonthly(screenId: InvestmentScreenId, month: YearMonth): F[Option[InvestmentEvaluation]] =
    collection
      .find(Filter.idEq(s"${screenId.value}:$month"))
      .first
      .mapOpt(_.evaluation)

  override def finishedScreenIds: F[Set[InvestmentScreenId]] =
    collection
      .aggregate[InvestmentEvaluationScreenReference](
        Aggregate.matchBy(Filter.eq(Field.Finished, true)).project(Projection.include(Field.ScreenId))
      )
      .all
      .mapList(_.screenId)
      .map(_.toSet)

  override def findFinished(screenId: InvestmentScreenId): F[Option[InvestmentEvaluation]] =
    collection
      .find(Filter.eq(Field.ScreenId, screenId.value) && Filter.eq(Field.Finished, true))
      .sort(Sort.desc(Field.Month))
      .first
      .mapOpt(_.evaluation)

  override def unsettled: F[List[InvestmentEvaluation]] =
    collection
      .find(Filter.eq(Field.Settled, false))
      .all
      .mapList(_.evaluation)

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
      collection = raw.withAddedCodec[InvestmentEvaluationScreenReference].withAddedCodec[InvestmentEvaluation]
      _ <- collection.createIndex(Index.ascending(Field.Settled))
      _ <- collection.createIndex(Index.ascending(List(Field.Finished, Field.ScreenId)))
    yield LiveInvestmentEvaluationRepository[F](collection)
