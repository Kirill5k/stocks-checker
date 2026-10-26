package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.all.*
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Filter, Index, Sort}
import mongo4cats.operations.Update
import mongo4cats.models.collection.FindOneAndUpdateOptions
import com.mongodb.client.model.ReturnDocument
import stockschecker.domain.*
import java.time.Instant

trait InvestmentScreenRepository[F[_]]:
  def save(run: InvestmentScreenRun): F[Unit]
  def find(id: String): F[Option[InvestmentScreenRun]]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def ids: F[List[String]]

object InvestmentScreenRepository extends MongoJsonCodecs:
  val CollectionName = "investment-screens"
  val InputCollectionName = "investment-screen-inputs"
  final case class Header(
      _id: String,
      createdAt: Instant,
      rankingId: String,
      request: InvestmentScreenRequest,
      cohorts: List[InvestmentCohort],
      summary: InvestmentScreenSummary
  ) derives Codec.AsObject

  def summary(run: InvestmentScreenRun): InvestmentScreenSummary =
    InvestmentScreenSummary(run.id, run.createdAt, run.recipe.version, run.inputs.size,
      run.scorecards.count(_.eligible), run.cohorts.find(_.name == "investment").fold(0)(_.tickers.size),
      run.scorecards.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _))

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentScreenRepository[F]] =
    for
      headers <- db.getCollectionWithCodec[Header](CollectionName)
      rankings <- InvestmentRankingRepository.make[F](db)
      _ <- headers.createIndex(Index.descending("createdAt"))
    yield new InvestmentScreenRepository[F]:
      def save(run: InvestmentScreenRun): F[Unit] =
        rankings.find(run.rankingId).flatMap { ranking =>
          MonadThrow[F].raiseWhen(ranking.isEmpty)(new IllegalArgumentException("Completed immutable ranking is required")) >>
            headers.insertOne(Header(run.id, run.createdAt, run.rankingId, run.request, run.cohorts, summary(run))).void
        }
      def find(id: String): F[Option[InvestmentScreenRun]] =
        headers.find(Filter.idEq(id)).first.flatMap {
          case None => none[InvestmentScreenRun].pure[F]
          case Some(h) => rankings.find(h.rankingId).map(_.map(r =>
            InvestmentScreenRun(h._id, h.createdAt, r.recipe, h.request, r.inputs, r.scorecards, h.cohorts, r.id)))
        }
      def list(limit: Int): F[List[InvestmentScreenSummary]] =
        headers.find.sort(Sort.desc("createdAt")).limit(limit).all.map(_.map(_.summary).toList)
      def ids: F[List[String]] = headers.find.sort(Sort.asc("createdAt")).all.map(_.map(_._id).toList)

trait InvestmentRankingRepository[F[_]]:
  def save(run: InvestmentRankingRun): F[Unit]
  def find(id: String): F[Option[InvestmentRankingRun]]
  def latest: F[Option[InvestmentRankingRun]]

object InvestmentRankingRepository extends MongoJsonCodecs:
  val CollectionName = "investment-rankings"
  val InputCollectionName = "investment-ranking-inputs"
  final case class Header(_id: String, createdAt: Instant, recipe: InvestmentRecipe) derives Codec.AsObject
  final case class Input(_id: String, rankingId: String, ordinal: Int, inputOrdinal: Int,
      candidate: InvestmentCandidate, scorecard: InvestmentScorecard) derives Codec.AsObject

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentRankingRepository[F]] =
    for
      headers <- db.getCollectionWithCodec[Header](CollectionName)
      inputs <- db.getCollectionWithCodec[Input](InputCollectionName)
      _ <- headers.createIndex(Index.descending("createdAt"))
      _ <- inputs.createIndex(Index.ascending("rankingId"))
    yield new InvestmentRankingRepository[F]:
      def save(run: InvestmentRankingRun): F[Unit] =
        val candidates = run.inputs.zipWithIndex.map { case (i, n) => i.stock.security.ticker -> (i, n) }.toMap
        val rows = run.scorecards.zipWithIndex.map { case (scorecard, i) =>
          val (candidate, inputOrdinal) = candidates(scorecard.ticker)
          Input(s"${run.id}:${scorecard.ticker.value}", run.id, i, inputOrdinal, candidate, scorecard)
        }
        // Each candidate is a separate document; the complete-market universe may exceed 16 MB.
        // Publishing the header last keeps interrupted computations invisible to readers.
        rows.grouped(100).toList.traverse_(batch => inputs.insertMany(batch).void) >>
          headers.insertOne(Header(run.id, run.createdAt, run.recipe)).void
      private def hydrate(h: Header): F[InvestmentRankingRun] =
        inputs.find(Filter.eq("rankingId", h._id)).sort(Sort.asc("ordinal")).all.map { rows =>
          InvestmentRankingRun(h._id, h.createdAt, h.recipe, rows.toList.sortBy(_.inputOrdinal).map(_.candidate), rows.map(_.scorecard).toList)
        }
      def find(id: String): F[Option[InvestmentRankingRun]] = headers.find(Filter.idEq(id)).first.flatMap(_.traverse(hydrate))
      def latest: F[Option[InvestmentRankingRun]] = headers.find.sort(Sort.desc("createdAt")).first.flatMap(_.traverse(hydrate))

trait InvestmentEvaluationRepository[F[_]]:
  def saveMonthly(evaluation: InvestmentEvaluation): F[Unit]
  def findMonthly(screenId: String, month: String): F[Option[InvestmentEvaluation]]
  def findCapturePlan(screenId: String, month: String): F[Option[EvaluationCapturePlan]]
  def getOrCreateCapturePlan(plan: EvaluationCapturePlan): F[EvaluationCapturePlan]
  def capturePlans: F[List[EvaluationCapturePlan]]

object InvestmentEvaluationRepository extends MongoJsonCodecs:
  val CollectionName = "investment-evaluations"
  final case class Entry(_id: String, screenId: String, month: String, evaluation: InvestmentEvaluation) derives Codec.AsObject
  final case class PlanEntry(_id: String, plan: EvaluationCapturePlan) derives Codec.AsObject
  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentEvaluationRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[Entry](CollectionName)
      plans <- db.getCollectionWithCodec[PlanEntry]("investment-evaluation-plans")
    yield
      new InvestmentEvaluationRepository[F]:
        def saveMonthly(evaluation: InvestmentEvaluation): F[Unit] =
          val month = evaluation.captureMonth.getOrElse(evaluation.evaluatedAt.toString.take(7))
          collection.insertOne(Entry(s"${evaluation.screenId}:$month", evaluation.screenId, month, evaluation)).void
        def findMonthly(screenId: String, month: String): F[Option[InvestmentEvaluation]] =
          collection.find(Filter.idEq(s"$screenId:$month")).first.map(_.map(_.evaluation))
        def findCapturePlan(screenId: String, month: String): F[Option[EvaluationCapturePlan]] =
          plans.find(Filter.idEq(s"$screenId:$month")).first.map(_.map(_.plan))
        def capturePlans: F[List[EvaluationCapturePlan]] = plans.find.all.map(_.toList.map(_.plan))
        def getOrCreateCapturePlan(plan: EvaluationCapturePlan): F[EvaluationCapturePlan] =
          plans.withAddedCodec[EvaluationCapturePlan].findOneAndUpdate(Filter.idEq(s"${plan.screenId}:${plan.month}"),
            Update.setOnInsert("plan", plan), FindOneAndUpdateOptions(upsert = true, returnDocument = ReturnDocument.AFTER))
            .flatMap(p => MonadThrow[F].fromOption(p.map(_.plan), new IllegalStateException("Evaluation capture plan was not stored")))
