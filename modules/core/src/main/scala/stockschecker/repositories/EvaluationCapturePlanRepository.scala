package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.mongodb.client.model.ReturnDocument
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.FindOneAndUpdateOptions
import mongo4cats.operations.{Filter, Index, Update}
import stockschecker.domain.{EvaluationCapturePlan, InvestmentScreenId}
import stockschecker.repositories.entities.InvestmentEvaluationPlanEntity

trait EvaluationCapturePlanRepository[F[_]]:
  def find(screenId: InvestmentScreenId, month: String): F[Option[EvaluationCapturePlan]]
  def getOrCreate(plan: EvaluationCapturePlan): F[EvaluationCapturePlan]
  def retire(screenId: InvestmentScreenId, month: String): F[Unit]
  def all: F[List[EvaluationCapturePlan]]

final private class LiveEvaluationCapturePlanRepository[F[_]](
    private val collection: MongoCollection[F, InvestmentEvaluationPlanEntity]
)(using
    F: MonadThrow[F]
) extends EvaluationCapturePlanRepository[F] {

  override def find(screenId: InvestmentScreenId, month: String): F[Option[EvaluationCapturePlan]] =
    collection.find(Filter.idEq(s"${screenId.value}:$month")).first.mapOpt(_.plan)

  override def getOrCreate(plan: EvaluationCapturePlan): F[EvaluationCapturePlan] =
    collection
      .findOneAndUpdate(
        Filter.idEq(s"${plan.screenId.value}:${plan.month}"),
        Update.setOnInsert("plan", plan),
        FindOneAndUpdateOptions(upsert = true, returnDocument = ReturnDocument.AFTER)
      )
      .flatMap(p => MonadThrow[F].fromOption(p.map(_.plan), new IllegalStateException("Evaluation capture plan was not stored")))

  override def retire(screenId: InvestmentScreenId, month: String): F[Unit] =
    collection.deleteOne(Filter.idEq(s"${screenId.value}:$month")).void

  override def all: F[List[EvaluationCapturePlan]] =
    collection.find.all.mapList(_.plan)
}

object EvaluationCapturePlanRepository extends MongoJsonCodecs:
  val CollectionName = "investment-evaluation-plans"

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[EvaluationCapturePlanRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[InvestmentEvaluationPlanEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending("plan.through"))
    yield LiveEvaluationCapturePlanRepository[F](collection.withAddedCodec[EvaluationCapturePlan])
