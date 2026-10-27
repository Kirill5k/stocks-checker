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

import java.time.{YearMonth, ZoneOffset}

trait InvestmentEvaluationRepository[F[_]]:
  def saveMonthly(evaluation: InvestmentEvaluation): F[Unit]
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
    val month    = evaluation.captureMonth.getOrElse(YearMonth.from(evaluation.evaluatedAt.atZone(ZoneOffset.UTC)))
    val id       = s"${evaluation.screenId.value}:$month"
    val writable = Filter.idEq(id) && Filter.ne(Field.settled, true)
    val update   = Update
      .set(Field.screenId, evaluation.screenId.value)
      .set(Field.month, month.toString)
      .set(Field.evaluation, evaluation)
      .set(Field.complete, evaluation.isComplete)
      .set(Field.settled, evaluation.settled)
      .set(Field.finished, evaluation.isFinished)
    collection.updateOne(writable, update, UpdateOptions(upsert = true)).void.handleErrorWith {
      // Another capture can insert first or settle between writes. Update an existing unfinished
      // document without upsert; a settled winner remains immutable.
      case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 =>
        collection.updateOne(writable, update).void
      case error => F.raiseError(error)
    }

  override def findMonthly(screenId: InvestmentScreenId, month: YearMonth): F[Option[InvestmentEvaluation]] =
    collection
      .find(Filter.idEq(s"${screenId.value}:$month"))
      .first
      .mapOpt(_.evaluation)

  override def finishedScreenIds: F[Set[InvestmentScreenId]] =
    collection
      .aggregate[InvestmentEvaluationScreenReference](
        Aggregate.matchBy(Filter.eq(Field.finished, true)).project(Projection.include(Field.screenId))
      )
      .all
      .mapList(_.screenId)
      .map(_.toSet)

  override def findFinished(screenId: InvestmentScreenId): F[Option[InvestmentEvaluation]] =
    collection
      .find(Filter.eq(Field.screenId, screenId.value) && Filter.eq(Field.finished, true))
      .sort(Sort.desc(Field.month))
      .first
      .mapOpt(_.evaluation)

  override def unsettled: F[List[InvestmentEvaluation]] =
    collection
      .find(Filter.eq(Field.settled, false))
      .all
      .mapList(_.evaluation)

}

object InvestmentEvaluationRepository extends MongoJsonCodecs:
  val CollectionName = "investment-evaluations"

  object Field:
    val screenId   = "screenId"
    val month      = "month"
    val evaluation = "evaluation"
    val complete   = "complete"
    val settled    = "settled"
    val finished   = "finished"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentEvaluationRepository[F]] =
    for
      raw <- db.getCollectionWithCodec[InvestmentEvaluationEntity](CollectionName)
      collection = raw.withAddedCodec[InvestmentEvaluationScreenReference].withAddedCodec[InvestmentEvaluation]
      _ <- collection.createIndex(Index.ascending(Field.settled))
      _ <- collection.createIndex(Index.ascending(List(Field.finished, Field.screenId)))
    yield LiveInvestmentEvaluationRepository[F](collection)
