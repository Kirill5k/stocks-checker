package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.UpdateOptions
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.*
import stockschecker.repositories.entities.{InvestmentEvaluationEntity, InvestmentEvaluationScreenReference}

trait InvestmentEvaluationRepository[F[_]]:
  def saveMonthly(evaluation: InvestmentEvaluation): F[Unit]
  def findMonthly(screenId: InvestmentScreenId, month: String): F[Option[InvestmentEvaluation]]
  def unsettled: F[List[InvestmentEvaluation]]
  def finishedScreenIds: F[Set[InvestmentScreenId]]
  def findFinished(screenId: InvestmentScreenId): F[Option[InvestmentEvaluation]]

final private class LiveInvestmentEvaluationRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentEvaluationEntity]
)(using
    F: MonadThrow[F]
) extends InvestmentEvaluationRepository[F] {

  override def saveMonthly(evaluation: InvestmentEvaluation): F[Unit] =
    val month = evaluation.captureMonth.getOrElse(evaluation.evaluatedAt.toString.take(7))
    val id    = s"${evaluation.screenId.value}:$month"
    collection.find(Filter.idEq(id)).first.flatMap {
      case Some(entry) if entry.evaluation.isSettled => F.unit
      case _                                         =>
        collection
          .updateOne(
            Filter.idEq(id) && Filter.ne("settled", true),
            Update
              .set("screenId", evaluation.screenId.value)
              .set("month", month)
              .set("evaluation", evaluation)
              .set("complete", evaluation.isComplete)
              .set("settled", evaluation.isSettled)
              .set("finished", evaluation.isFinished),
            UpdateOptions(upsert = true)
          )
          .void
          .handleErrorWith {
            case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 =>
              collection.find(Filter.idEq(id)).first.flatMap {
                case Some(entry) if entry.evaluation.isSettled => F.unit
                case _                                         => MonadThrow[F].raiseError(error)
              }
            case error => MonadThrow[F].raiseError(error)
          }
    }

  override def findMonthly(screenId: InvestmentScreenId, month: String): F[Option[InvestmentEvaluation]] =
    collection
      .find(Filter.idEq(s"${screenId.value}:$month"))
      .first
      .mapOpt(_.evaluation)

  override def finishedScreenIds: F[Set[InvestmentScreenId]] =
    collection
      .aggregate[InvestmentEvaluationScreenReference](
        Aggregate.matchBy(Filter.eq("finished", true)).project(Projection.include("screenId"))
      )
      .all
      .mapList(_.screenId)
      .map(_.toSet)

  override def findFinished(screenId: InvestmentScreenId): F[Option[InvestmentEvaluation]] =
    collection
      .find(Filter.eq("screenId", screenId.value) && Filter.eq("finished", true))
      .sort(Sort.desc("month"))
      .first
      .mapOpt(_.evaluation)

  override def unsettled: F[List[InvestmentEvaluation]] =
    collection
      .find(Filter.ne("settled", true))
      .all
      .flatMap { entries =>
        val (settledEntries, pending) = entries.toList.partition(entry => entry.evaluation.isSettled)
        settledEntries
          .traverse_ { entry =>
            collection
              .updateOne(
                Filter.idEq(entry._id) && Filter.ne("settled", true),
                Update.set("complete", entry.evaluation.isComplete).set("settled", true).set("finished", entry.evaluation.isFinished)
              )
              .void
          }
          .as(pending.map(_.evaluation))
      }
}

object InvestmentEvaluationRepository extends MongoJsonCodecs:
  val CollectionName = "investment-evaluations"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentEvaluationRepository[F]] =
    for
      raw <- db.getCollectionWithCodec[InvestmentEvaluationEntity](CollectionName)
      collection = raw.withAddedCodec[InvestmentEvaluationScreenReference].withAddedCodec[InvestmentEvaluation]
      _ <- collection.createIndex(Index.ascending("settled"))
      _ <- collection.createIndex(Index.ascending(List("finished", "screenId")))
    yield LiveInvestmentEvaluationRepository[F](collection)
