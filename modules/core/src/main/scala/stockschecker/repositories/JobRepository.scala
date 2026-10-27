package stockschecker.repositories

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.mongodb.client.model.{Indexes, ReturnDocument}
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{FindOneAndUpdateOptions, UpdateOptions}
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.actions.Action
import stockschecker.domain.{CommandId, Job, JobId, JobPlan, JobState, JobStatusSummary}
import stockschecker.repositories.entities.{JobEntity, JobIdEntity, JobStateCount}

import java.time.Instant
import java.util.UUID
import java.util.regex.Pattern
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

trait JobRepository[F[_]] {
  def enqueue(job: Job): F[Unit]
  def find(id: JobId): F[Option[Job]]
  def summarize(ids: List[JobId]): F[JobStatusSummary]
  def terminalIds(ids: List[JobId]): F[Set[JobId]]
  def claim(now: Instant, lease: FiniteDuration): F[Option[Job]]
  def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean]
  def finish(job: Job, state: JobState, error: Option[String] = None): F[Unit]
  def savePlan(job: Job, plan: JobPlan): F[Boolean]
  def waitForChildren(job: Job, children: List[JobId], now: Instant): F[Unit]
  def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean): F[Unit]
  def pause(job: Job, provider: String, error: String): F[Unit]
  def resumePaused(provider: String, now: Instant): F[Unit]
  def retryTerminal(id: JobId, now: Instant): F[Boolean]
  def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit]
  def pruneCompleted(now: Instant): F[Unit]
}

final private class LiveJobRepository[F[_]](
    collection: MongoCollection[F, JobEntity]
)(using
    F: Async[F]
) extends JobRepository[F] {
  import JobRepository.{ClaimIndexFields, Retention}

  private def owned(job: Job): Filter =
    Filter.idEq(job.id.value) && Filter.eq("job.state", Job.Running.value) && Filter.eq("job.leaseToken", job.leaseToken)

  private def release: Update = Update.set("job.leaseToken", Option.empty[String]).set("job.leaseUntil", Option.empty[Instant])

  private val terminal = Filter.in("job.state", Job.terminalStates.map(_.value))

  override def enqueue(job: Job): F[Unit] =
    collection
      .updateOne(
        Filter.idEq(job.id.value),
        Update
          .setOnInsert("job", job)
          .setOnInsert(
            "priority",
            job.action match {
              case _: Action.RefreshEvaluationPrices => 1
              case _                                 => 0
            }
          ),
        UpdateOptions(upsert = true)
      )
      .void

  override def find(id: JobId): F[Option[Job]] = collection.find(Filter.idEq(id.value)).first.map(_.map(_.job))

  override def summarize(ids: List[JobId]): F[JobStatusSummary] =
    if (ids.isEmpty) JobStatusSummary(Map.empty).pure[F]
    else
      collection
        .aggregate[JobStateCount](
          Aggregate
            .matchBy(Filter.in("_id", ids.map(_.value)))
            .group("$job.state", Accumulator.sum("count", 1))
        )
        .all
        .map(rows => JobStatusSummary(rows.map(row => row._id -> row.count).toMap))

  override def terminalIds(ids: List[JobId]): F[Set[JobId]] =
    if (ids.isEmpty) Set.empty[JobId].pure[F]
    else
      collection
        .aggregate[JobIdEntity](
          Aggregate
            .matchBy(Filter.in("_id", ids.map(_.value)) && terminal)
            .project(Projection.include("_id"))
        )
        .all
        .map(_.map(_._id).toSet)

  override def claim(now: Instant, lease: FiniteDuration): F[Option[Job]] =
    Async[F].delay(UUID.randomUUID().toString).flatMap { token =>
      val ready   = Filter.in("job.state", List(Job.Pending.value, Job.Waiting.value)) && Filter.lte("job.availableAt", now)
      val expired = Filter.eq("job.state", Job.Running.value) && Filter.lte("job.leaseUntil", now)
      collection
        .findOneAndUpdate(
          ready || expired,
          Update.set("job.state", Job.Running.value).set("job.leaseToken", token).set("job.leaseUntil", now.plusMillis(lease.toMillis)),
          FindOneAndUpdateOptions(
            returnDocument = ReturnDocument.AFTER,
            sort = Some(Sort.asc("priority", "job.availableAt", "job.createdAt", "_id"))
            // Without this hint Mongo may choose the lease index and sort the entire ready backlog.
            // The ordered index merges the three state scans and stops after the first eligible job.
          ).hint(Indexes.ascending(ClaimIndexFields.asJava))
        )
        .map(_.map(_.job))
    }

  override def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean] =
    collection.updateOne(owned(job), Update.set("job.leaseUntil", now.plusMillis(lease.toMillis))).map(_.getMatchedCount == 1)

  override def finish(job: Job, state: JobState, error: Option[String]): F[Unit] =
    collection.updateOne(owned(job), release.set("job.state", state.value).set("job.error", error).currentDate("job.completedAt")).void

  override def savePlan(job: Job, plan: JobPlan): F[Boolean] =
    collection.updateOne(owned(job), Update.set("job.plan", plan)).map(_.getMatchedCount == 1)

  override def waitForChildren(job: Job, children: List[JobId], now: Instant): F[Unit] =
    collection
      .updateOne(
        owned(job),
        release
          .set("job.state", Job.Waiting.value)
          .set("job.children", children.map(_.value))
          .set("job.availableAt", now.plusSeconds(job.nextPollDelaySeconds))
          .set("job.waitCount", job.waitCount.getOrElse(0) + 1)
      )
      .void

  override def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean): F[Unit] =
    collection
      .updateOne(
        owned(job),
        release
          .set("job.state", Job.Pending.value)
          .set("job.availableAt", until)
          .set("job.error", error)
          .set("job.retries", job.retries + (if (countRetry) 1 else 0))
          .set("job.waitCount", job.waitCount.getOrElse(0) + 1)
      )
      .void

  override def pause(job: Job, provider: String, error: String): F[Unit] =
    collection
      .updateOne(owned(job), release.set("job.state", Job.Paused.value).set("job.pausedProvider", provider).set("job.error", error))
      .void

  override def resumePaused(provider: String, now: Instant): F[Unit] =
    collection
      .updateMany(
        Filter.eq("job.state", Job.Paused.value) && Filter.eq("job.pausedProvider", provider),
        Update.set("job.state", Job.Pending.value).set("job.availableAt", now).set("job.pausedProvider", Option.empty[String])
      )
      .void

  override def retryTerminal(id: JobId, now: Instant): F[Boolean] =
    find(id).flatMap {
      case Some(job)
          if job.isTerminal && job.children.isEmpty && job.plan.isEmpty && !job.id.value.contains("/") &&
            job.action.isInstanceOf[Action.RefreshEvaluationPrices] && job.restartCount.getOrElse(0) < Job.MaxEvaluationRestarts =>
        val retryAt          = job.completedAt.fold(now)(_.plusSeconds(1.day.toSeconds))
        val sameRestartCount = job.restartCount.fold(Filter.isNull("job.restartCount"))(count => Filter.eq("job.restartCount", count))
        collection
          .updateOne(
            Filter.idEq(id.value) && Filter.eq("job.state", job.state.value) && Filter
              .eq("job.completedAt", job.completedAt) && sameRestartCount,
            release
              .set("job.state", Job.Pending.value)
              .set("job.availableAt", if (retryAt.isAfter(now)) retryAt else now)
              .set("job.retries", 0)
              .set("job.restartCount", job.restartCount.getOrElse(0) + 1)
              .set("job.error", Option.empty[String])
              .set("job.completedAt", Option.empty[Instant])
          )
          .map(_.getMatchedCount == 1)
      case _ => false.pure[F]
    }

  override def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit] =
    collection
      .updateMany(
        Filter.eq("job.command.commandId", commandId.value) && Filter.lt("job.command.version", currentVersion) &&
          Filter.in("job.state", List(Job.Pending.value, Job.Waiting.value, Job.Paused.value)),
        release.set("job.state", Job.Cancelled.value).set("job.error", "Command changed or deactivated").currentDate("job.completedAt")
      )
      .void

  override def pruneCompleted(now: Instant): F[Unit] = {
    val expired = terminal && Filter.lte("job.completedAt", now.minusSeconds(Retention.toSeconds))
    // Fan-out IDs are rooted at '<root>/<index>'. Retain every child until its root has finished,
    // even when a provider budget leaves that root waiting for weeks.
    collection
      .aggregate[JobIdEntity](
        Aggregate
          .matchBy(expired && Filter.regex("_id", "^[^/]+$"))
          .lookup(CommandRepository.CollectionName, "_id", "activeRunId", "activeCommands")
          .matchBy(Filter.notExists("activeCommands.0"))
          // A capture can wait on other tickers for longer than the job retention window.
          // Keep this cutoff's attempt counters until all capture plans using it are settled.
          .lookup(EvaluationCapturePlanRepository.CollectionName, "job.action.through", "plan.through", "activeCaptures")
          .matchBy(Filter.notExists("activeCaptures.0"))
          .project(Projection.include("_id"))
      )
      .stream
      .evalMap { root =>
        // Keep the root as a recovery marker until its descendants have gone. Only standalone
        // leaf jobs can be retried, so a terminal fan-out cannot be revived during this cleanup.
        collection.deleteMany(Filter.regex("_id", s"^${Pattern.quote(root._id.value)}/")).void >>
          collection.deleteOne(Filter.idEq(root._id.value) && expired).void
      }
      .compile
      .drain
  }
}

object JobRepository extends MongoJsonCodecs {
  val CollectionName                         = "jobs"
  val Retention                              = 30.days
  private[repositories] val ClaimIndexFields = List("job.state", "priority", "job.availableAt", "job.createdAt", "_id")
  // The API codec encodes Instants as JSON strings; Mongo claims and updates use BSON dates.
  private given Codec.AsObject[Job] = Codec.AsObject.derived[Job]

  def make[F[_]: Async](db: MongoDatabase[F]): F[JobRepository[F]] =
    for {
      collection <- db.getCollectionWithCodec[JobEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(ClaimIndexFields))
      _          <- collection.createIndex(Index.ascending(List("job.state", "job.leaseUntil")))
      _          <- collection.createIndex(Index.ascending("job.command.commandId"))
      _          <- collection.createIndex(Index.ascending(List("job.state", "job.completedAt")))
      _          <- collection.updateMany(Filter.isNull("priority"), Update.set("priority", 0))
      _          <- collection.updateMany(Filter.eq("job.action.kind", "refresh-evaluation-prices"), Update.set("priority", 1))
      // Start the retention clock for outcomes written before completion timestamps were introduced.
      _ <- collection.updateMany(
        Filter.in("job.state", Job.terminalStates.map(_.value)) && Filter.isNull("job.completedAt"),
        Update.currentDate("job.completedAt")
      )
    } yield new LiveJobRepository(
      collection.withAddedCodec[Job].withAddedCodec[JobPlan].withAddedCodec[JobStateCount].withAddedCodec[JobIdEntity]
    )
}
