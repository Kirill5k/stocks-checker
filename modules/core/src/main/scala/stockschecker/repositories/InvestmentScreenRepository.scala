package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.database.MongoDatabase
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Sort}
import mongo4cats.operations.Update
import mongo4cats.models.collection.FindOneAndUpdateOptions
import com.mongodb.client.model.ReturnDocument
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import java.time.Instant

trait InvestmentScreenRepository[F[_]]:
  def save(run: InvestmentScreenRun): F[Unit]
  def find(id: InvestmentScreenId): F[InvestmentScreenRun]
  def header(id: InvestmentScreenId): F[Option[InvestmentScreenRepository.Header]]
  def list(limit: Int): F[List[InvestmentScreenSummary]]
  def ids: F[List[InvestmentScreenId]]

object InvestmentScreenRepository extends MongoJsonCodecs:
  val CollectionName = "investment-screens"
  final case class Header(
      _id: InvestmentScreenId,
      createdAt: Instant,
      rankingId: String,
      request: InvestmentScreenRequest,
      cohorts: List[InvestmentCohort],
      summary: InvestmentScreenSummary
  ) derives Codec.AsObject

  def summary(run: InvestmentScreenRun): InvestmentScreenSummary =
    InvestmentScreenSummary(
      run.id,
      run.createdAt,
      run.recipe.version,
      run.inputs.size,
      run.scorecards.count(_.eligible),
      run.cohorts.find(_.name == "investment").fold(0)(_.tickers.size),
      run.scorecards.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _)
    )

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentScreenRepository[F]] =
    for
      headers  <- db.getCollectionWithCodec[Header](CollectionName)
      rankings <- InvestmentRankingRepository.make[F](db)
      _        <- headers.createIndex(Index.descending("createdAt"))
    yield new InvestmentScreenRepository[F]:
      def save(run: InvestmentScreenRun): F[Unit] =
        rankings.header(run.rankingId).flatMap { ranking =>
          MonadThrow[F].raiseWhen(ranking.isEmpty)(new IllegalArgumentException("Completed immutable ranking is required")) >>
            headers.insertOne(Header(run.id, run.createdAt, run.rankingId, run.request, run.cohorts, summary(run))).void
        }
      def header(id: InvestmentScreenId): F[Option[Header]]    = headers.find(Filter.idEq(id.value)).first
      def find(id: InvestmentScreenId): F[InvestmentScreenRun] =
        val notFound = AppError.EntityDoesNotExist("Investment screen", id.value)
        for
          h <- header(id).flatMap(_.liftTo[F](notFound))
          r <- rankings.find(h.rankingId).flatMap(_.liftTo[F](notFound))
        yield InvestmentScreenRun(h._id, h.createdAt, r.recipe, h.request, r.inputs, r.scorecards, h.cohorts, r.id)
      def list(limit: Int): F[List[InvestmentScreenSummary]] =
        headers.find.sort(Sort.desc("createdAt")).limit(limit).all.map(_.map(_.summary).toList)
      def ids: F[List[InvestmentScreenId]] = headers.find.sort(Sort.asc("createdAt")).all.map(_.map(_._id).toList)

trait InvestmentRankingRepository[F[_]]:
  def save(run: InvestmentRankingRun): F[Unit]
  def find(id: String): F[Option[InvestmentRankingRun]]
  def latest: F[Option[InvestmentRankingRun]]
  def header(id: String): F[Option[InvestmentRankingRepository.Header]]
  def latestHeader: F[Option[InvestmentRankingRepository.Header]]
  def prune(before: Instant): F[Unit]

object InvestmentRankingRepository extends MongoJsonCodecs:
  val CollectionName      = "investment-rankings"
  val InputCollectionName = "investment-ranking-inputs"
  final case class Header(_id: String, createdAt: Instant, recipe: InvestmentRecipe, summary: InvestmentRankingSummary)
      derives Codec.AsObject
  final private case class StoredHeader(
      _id: String,
      createdAt: Instant,
      recipe: InvestmentRecipe,
      summary: Option[InvestmentRankingSummary]
  ) derives Codec.AsObject
  final private case class Outcome(eligible: Boolean, exclusions: List[String]) derives Codec.AsObject
  final private case class RankingReference(rankingId: String) derives Codec.AsObject
  final private case class RankingId(_id: String) derives Codec.AsObject

  def summary(run: InvestmentRankingRun): InvestmentRankingSummary =
    InvestmentRankingSummary(
      run.inputs.size,
      run.scorecards.count(_.eligible),
      run.scorecards.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _)
    )

  private def compact(candidate: InvestmentCandidate): InvestmentCandidate =
    candidate.copy(financialSnapshot =
      candidate.financialSnapshot.map(snapshot =>
        snapshot.copy(
          metrics = snapshot.metrics.filter { case (metric, _) => Set("peTTM", "pfcfShareTTM", "epsTTM")(metric) },
          annual = Nil,
          quarterly = Nil,
          rawPayload = ""
        )
      )
    )
  final case class Input(
      _id: String,
      rankingId: String,
      ordinal: Int,
      inputOrdinal: Int,
      candidate: InvestmentCandidate,
      scorecard: InvestmentScorecard,
      createdAt: Option[Instant] = None
  ) derives Codec.AsObject

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentRankingRepository[F]] =
    for
      rawHeaders <- db.getCollectionWithCodec[StoredHeader](CollectionName)
      headers = rawHeaders.withAddedCodec[RankingId]
      rawInputs <- db.getCollectionWithCodec[Input](InputCollectionName)
      inputs = rawInputs.withAddedCodec[Outcome]
      screens <- db.getCollectionWithCodec[RankingReference](InvestmentScreenRepository.CollectionName)
      _       <- headers.createIndex(Index.descending("createdAt"))
      _       <- inputs.createIndex(Index.ascending("rankingId"))
    yield new InvestmentRankingRepository[F]:
      def save(run: InvestmentRankingRun): F[Unit] =
        val candidates = run.inputs.zipWithIndex.map { case (i, n) => i.stock.security.ticker -> (i, n) }.toMap
        val rows       = run.scorecards.zipWithIndex.map { case (scorecard, i) =>
          val (candidate, inputOrdinal) = candidates(scorecard.ticker)
          Input(s"${run.id}:${scorecard.ticker.value}", run.id, i, inputOrdinal, compact(candidate), scorecard, Some(run.createdAt))
        }
        // Each candidate is a separate document; the complete-market universe may exceed 16 MB.
        // Publishing the header last keeps interrupted computations invisible to readers.
        rows.grouped(100).toList.traverse_(batch => inputs.insertMany(batch).void) >>
          headers.insertOne(StoredHeader(run.id, run.createdAt, run.recipe, Some(summary(run)))).void
      private def hydrate(h: Header): F[InvestmentRankingRun] =
        inputs.find(Filter.eq("rankingId", h._id)).sort(Sort.asc("ordinal")).all.map { rows =>
          InvestmentRankingRun(
            h._id,
            h.createdAt,
            h.recipe,
            rows.toList.sortBy(_.inputOrdinal).map(_.candidate),
            rows.map(_.scorecard).toList
          )
        }
      private def published(h: StoredHeader): F[Header] =
        h.summary.fold(
          inputs
            .aggregate[Outcome](
              Aggregate
                .matchBy(Filter.eq("rankingId", h._id))
                .project(
                  Projection.exclude("_id").computed("eligible", "$scorecard.eligible").computed("exclusions", "$scorecard.exclusions")
                )
            )
            .all
            .flatMap { outcomes =>
              val summary = InvestmentRankingSummary(
                outcomes.size,
                outcomes.count(_.eligible),
                outcomes.toList.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _)
              )
              headers
                .withAddedCodec[InvestmentRankingSummary]
                .updateOne(Filter.idEq(h._id), Update.set("summary", summary))
                .as(Header(h._id, h.createdAt, h.recipe, summary))
            }
        )(summary => Header(h._id, h.createdAt, h.recipe, summary).pure[F])
      def header(id: String): F[Option[Header]] = headers.find(Filter.idEq(id)).first.flatMap(_.traverse(published))
      def latestHeader: F[Option[Header]]       = headers.find.sort(Sort.desc("createdAt")).first.flatMap(_.traverse(published))
      def prune(before: Instant): F[Unit]       =
        for
          references <- screens.aggregate[RankingReference](Aggregate.project(Projection.include("rankingId"))).all
          obsolete   <- headers
            .aggregate[RankingId](
              Aggregate
                .matchBy(
                  Filter.lt("createdAt", before) &&
                    Filter.nin("_id", references.map(_.rankingId).toList)
                )
                .project(Projection.include("_id"))
            )
            .all
          _ <-
            if obsolete.isEmpty then ().pure[F]
            else
              inputs.deleteMany(Filter.in("rankingId", obsolete.map(_._id).toList)).void >>
                headers.deleteMany(Filter.in("_id", obsolete.map(_._id).toList)).void
          published <- headers.aggregate[RankingId](Aggregate.project(Projection.include("_id"))).all
          _         <- inputs.deleteMany(
            Filter.nin("rankingId", published.map(_._id).toList) &&
              (Filter.lt("createdAt", before) || Filter.isNull("createdAt"))
          )
        yield ()
      def find(id: String): F[Option[InvestmentRankingRun]] = header(id).flatMap(_.traverse(hydrate))
      def latest: F[Option[InvestmentRankingRun]]           = latestHeader.flatMap(_.traverse(hydrate))

trait InvestmentEvaluationRepository[F[_]]:
  def saveMonthly(evaluation: InvestmentEvaluation): F[Unit]
  def findMonthly(screenId: InvestmentScreenId, month: String): F[Option[InvestmentEvaluation]]
  def findCapturePlan(screenId: InvestmentScreenId, month: String): F[Option[EvaluationCapturePlan]]
  def getOrCreateCapturePlan(plan: EvaluationCapturePlan): F[EvaluationCapturePlan]
  def retireCapturePlan(screenId: InvestmentScreenId, month: String): F[Unit]
  def capturePlans: F[List[EvaluationCapturePlan]]
  def unsettled: F[List[InvestmentEvaluation]]
  def finishedScreenIds: F[Set[InvestmentScreenId]]
  def findFinished(screenId: InvestmentScreenId): F[Option[InvestmentEvaluation]]

object InvestmentEvaluationRepository extends MongoJsonCodecs:
  val CollectionName     = "investment-evaluations"
  val PlanCollectionName = "investment-evaluation-plans"
  final case class Entry(
      _id: String,
      screenId: InvestmentScreenId,
      month: String,
      evaluation: InvestmentEvaluation,
      complete: Option[Boolean] = None,
      settled: Option[Boolean] = None,
      finished: Option[Boolean] = None
  ) derives Codec.AsObject
  final private case class ScreenReference(screenId: InvestmentScreenId) derives Codec.AsObject
  final case class PlanEntry(_id: String, plan: EvaluationCapturePlan) derives Codec.AsObject
  def complete(evaluation: InvestmentEvaluation): Boolean =
    evaluation.pendingDataJobs.isEmpty && evaluation.dataThrough.nonEmpty && evaluation.horizons.nonEmpty &&
      evaluation.horizons.forall(h =>
        h.entryDate.nonEmpty && h.cohorts.nonEmpty && h.cohorts.forall(c => c.status != "incomplete" && c.missing.isEmpty)
      )

  // Legacy reports without a lifecycle marker are final only when all required data is auditable.
  def settled(evaluation: InvestmentEvaluation): Boolean  = evaluation.settled.getOrElse(complete(evaluation))
  def finished(evaluation: InvestmentEvaluation): Boolean =
    settled(evaluation) && evaluation.horizons.nonEmpty &&
      evaluation.dataThrough.exists(through =>
        evaluation.horizons.forall { horizon =>
          val lastPossibleExit = horizon.targetDate.plusDays(if horizon.entryDate.isEmpty then 14 else 7)
          horizon.exitDate.exists(date => !date.isAfter(through)) || !lastPossibleExit.isAfter(through)
        }
      )

  def make[F[_]: MonadThrow](db: MongoDatabase[F]): F[InvestmentEvaluationRepository[F]] =
    for
      raw <- db.getCollectionWithCodec[Entry](CollectionName)
      collection = raw.withAddedCodec[ScreenReference]
      plans <- db.getCollectionWithCodec[PlanEntry](PlanCollectionName)
      _     <- plans.createIndex(Index.ascending("plan.through"))
      _     <- collection.createIndex(Index.ascending("settled"))
      _     <- collection.createIndex(Index.ascending(List("finished", "screenId")))
    yield new InvestmentEvaluationRepository[F]:
      def saveMonthly(evaluation: InvestmentEvaluation): F[Unit] =
        val month   = evaluation.captureMonth.getOrElse(evaluation.evaluatedAt.toString.take(7))
        val id      = s"${evaluation.screenId.value}:$month"
        val persist = collection.find(Filter.idEq(id)).first.flatMap {
          case Some(entry) if settled(entry.evaluation) => ().pure[F]
          case _                                        =>
            collection
              .withAddedCodec[InvestmentEvaluation]
              .updateOne(
                Filter.idEq(id) && Filter.ne("settled", true),
                Update
                  .set("screenId", evaluation.screenId.value)
                  .set("month", month)
                  .set("evaluation", evaluation)
                  .set("complete", complete(evaluation))
                  .set("settled", settled(evaluation))
                  .set("finished", finished(evaluation)),
                mongo4cats.models.collection.UpdateOptions(upsert = true)
              )
              .void
              .handleErrorWith {
                case error: com.mongodb.MongoWriteException if error.getError.getCode == 11000 =>
                  collection.find(Filter.idEq(id)).first.flatMap {
                    case Some(entry) if settled(entry.evaluation) => ().pure[F]
                    case _                                        => MonadThrow[F].raiseError(error)
                  }
                case error => MonadThrow[F].raiseError(error)
              }
        }
        persist >> (if settled(evaluation) then retireCapturePlan(evaluation.screenId, month) else ().pure[F])
      def findMonthly(screenId: InvestmentScreenId, month: String): F[Option[InvestmentEvaluation]] =
        collection.find(Filter.idEq(s"${screenId.value}:$month")).first.map(_.map(_.evaluation))
      def retireCapturePlan(screenId: InvestmentScreenId, month: String): F[Unit] =
        plans.deleteOne(Filter.idEq(s"${screenId.value}:$month")).void
      def finishedScreenIds: F[Set[InvestmentScreenId]] =
        collection
          .aggregate[ScreenReference](Aggregate.matchBy(Filter.eq("finished", true)).project(Projection.include("screenId")))
          .all
          .map(_.map(_.screenId).toSet)
      def findFinished(screenId: InvestmentScreenId): F[Option[InvestmentEvaluation]] =
        collection
          .find(Filter.eq("screenId", screenId.value) && Filter.eq("finished", true))
          .sort(Sort.desc("month"))
          .first
          .map(_.map(_.evaluation))
      def findCapturePlan(screenId: InvestmentScreenId, month: String): F[Option[EvaluationCapturePlan]] =
        plans.find(Filter.idEq(s"${screenId.value}:$month")).first.map(_.map(_.plan))
      def capturePlans: F[List[EvaluationCapturePlan]] = plans.find.all.map(_.toList.map(_.plan))
      def unsettled: F[List[InvestmentEvaluation]]     =
        collection.find(Filter.ne("settled", true)).all.flatMap { entries =>
          val (settledEntries, pending) = entries.toList.partition(entry => settled(entry.evaluation))
          settledEntries
            .traverse_ { entry =>
              collection
                .updateOne(
                  Filter.idEq(entry._id) && Filter.ne("settled", true),
                  Update.set("complete", complete(entry.evaluation)).set("settled", true).set("finished", finished(entry.evaluation))
                )
                .void >>
                retireCapturePlan(entry.screenId, entry.month)
            }
            .as(pending.map(_.evaluation))
        }
      def getOrCreateCapturePlan(plan: EvaluationCapturePlan): F[EvaluationCapturePlan] =
        plans
          .withAddedCodec[EvaluationCapturePlan]
          .findOneAndUpdate(
            Filter.idEq(s"${plan.screenId.value}:${plan.month}"),
            Update.setOnInsert("plan", plan),
            FindOneAndUpdateOptions(upsert = true, returnDocument = ReturnDocument.AFTER)
          )
          .flatMap(p => MonadThrow[F].fromOption(p.map(_.plan), new IllegalStateException("Evaluation capture plan was not stored")))
