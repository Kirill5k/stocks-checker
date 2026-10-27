package stockschecker.repositories

import stockschecker.domain.Provider
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
import stockschecker.domain.{CommandId, Job, JobId, JobPlan, JobState, JobStatusSummary}
import stockschecker.repositories.entities.{JobEntity, JobIdEntity, JobStateCount}

import java.time.Instant
import java.util.UUID
import java.util.regex.Pattern
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

trait JobRepository[F[_]] {
  def enqueue(job: Job, priority: Int): F[Unit]
  def find(id: JobId): F[Option[Job]]
  def summarize(ids: List[JobId]): F[JobStatusSummary]
  def pendingIds(ids: List[JobId]): F[Set[JobId]]
  def claim(now: Instant, lease: FiniteDuration): F[Option[Job]]
  def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean]
  def finish(job: Job, state: JobState, error: Option[String] = None): F[Unit]
  def savePlan(job: Job, plan: JobPlan): F[Boolean]
  def waitForChildren(job: Job, children: List[JobId], availableAt: Instant): F[Unit]
  def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean): F[Unit]
  def pause(job: Job, provider: Provider, error: String): F[Unit]
  def resumePaused(provider: Provider, now: Instant): F[Unit]
  def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit]
  def pruneCompleted(now: Instant, protectedIds: Set[JobId]): F[Unit]
}

final private class LiveJobRepository[F[_]](
    collection: MongoCollection[F, JobEntity]
)(using
    F: Async[F]
) extends JobRepository[F] {
  import JobRepository.{ClaimIndexFields, Field, Retention}

  private def owned(job: Job): Filter =
    Filter.idEq(job.id.value) && Filter.eq(Field.state, Job.Running.value) && Filter.eq(Field.leaseToken, job.leaseToken)

  private def release: Update = Update.set(Field.leaseToken, Option.empty[String]).set(Field.leaseUntil, Option.empty[Instant])

  private val terminal = Filter.in(Field.state, Job.terminalStates.map(_.value))

  override def enqueue(job: Job, priority: Int): F[Unit] =
    collection
      .updateOne(
        Filter.idEq(job.id.value),
        Update
          .setOnInsert(Field.job, job)
          .setOnInsert(Field.priority, priority),
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
            .matchBy(Filter.in(Field.id, ids.map(_.value)))
            .group("$" + Field.state, Accumulator.sum(Field.count, 1))
        )
        .all
        .map(rows => JobStatusSummary(rows.map(row => row._id -> row.count).toMap))

  override def pendingIds(ids: List[JobId]): F[Set[JobId]] =
    if ids.isEmpty then Set.empty[JobId].pure[F]
    else
      collection
        .aggregate[JobIdEntity](
          Aggregate
            .matchBy(Filter.in(Field.id, ids.map(_.value)) && Filter.nin(Field.state, Job.terminalStates.map(_.value)))
            .project(Projection.include(Field.id))
        )
        .all
        .map(_.map(_._id).toSet)

  override def claim(now: Instant, lease: FiniteDuration): F[Option[Job]] =
    Async[F].delay(UUID.randomUUID().toString).flatMap { token =>
      val ready   = Filter.in(Field.state, List(Job.Pending.value, Job.Waiting.value)) && Filter.lte(Field.availableAt, now)
      val expired = Filter.eq(Field.state, Job.Running.value) && Filter.lte(Field.leaseUntil, now)
      collection
        .findOneAndUpdate(
          ready || expired,
          Update.set(Field.state, Job.Running.value).set(Field.leaseToken, token).set(Field.leaseUntil, now.plusMillis(lease.toMillis)),
          FindOneAndUpdateOptions(
            returnDocument = ReturnDocument.AFTER,
            sort = Some(Sort.asc(Field.priority, Field.availableAt, Field.createdAt, Field.id))
            // Without this hint Mongo may choose the lease index and sort the entire ready backlog.
            // The ordered index merges the three state scans and stops after the first eligible job.
          ).hint(Indexes.ascending(ClaimIndexFields.asJava))
        )
        .map(_.map(_.job))
    }

  override def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean] =
    collection.updateOne(owned(job), Update.set(Field.leaseUntil, now.plusMillis(lease.toMillis))).map(_.getMatchedCount == 1)

  override def finish(job: Job, state: JobState, error: Option[String]): F[Unit] =
    collection.updateOne(owned(job), release.set(Field.state, state.value).set(Field.error, error).currentDate(Field.completedAt)).void

  override def savePlan(job: Job, plan: JobPlan): F[Boolean] =
    collection.updateOne(owned(job), Update.set(Field.plan, plan)).map(_.getMatchedCount == 1)

  override def waitForChildren(job: Job, children: List[JobId], availableAt: Instant): F[Unit] =
    collection
      .updateOne(
        owned(job),
        release
          .set(Field.state, Job.Waiting.value)
          .set(Field.children, children.map(_.value))
          .set(Field.availableAt, availableAt)
          .set(Field.waitCount, job.waitCount + 1)
      )
      .void

  override def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean): F[Unit] =
    collection
      .updateOne(
        owned(job),
        release
          .set(Field.state, Job.Pending.value)
          .set(Field.availableAt, until)
          .set(Field.error, error)
          .set(Field.retries, job.retries + (if (countRetry) 1 else 0))
          .set(Field.waitCount, job.waitCount + 1)
      )
      .void

  override def pause(job: Job, provider: Provider, error: String): F[Unit] =
    collection
      .updateOne(owned(job), release.set(Field.state, Job.Paused.value).set(Field.pausedProvider, provider.print).set(Field.error, error))
      .void

  override def resumePaused(provider: Provider, now: Instant): F[Unit] =
    collection
      .updateMany(
        Filter.eq(Field.state, Job.Paused.value) && Filter.eq(Field.pausedProvider, provider.print),
        Update.set(Field.state, Job.Pending.value).set(Field.availableAt, now).set(Field.pausedProvider, Option.empty[Provider])
      )
      .void

  override def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit] =
    collection
      .updateMany(
        Filter.eq(Field.commandId, commandId.value) && Filter.lt(Field.commandVersion, currentVersion) &&
          Filter.in(Field.state, List(Job.Pending.value, Job.Waiting.value, Job.Paused.value)),
        release.set(Field.state, Job.Cancelled.value).set(Field.error, "Command changed or deactivated").currentDate(Field.completedAt)
      )
      .void

  override def pruneCompleted(now: Instant, protectedIds: Set[JobId]): F[Unit] = {
    val expired = terminal && Filter.lte(Field.completedAt, now.minusSeconds(Retention.toSeconds))
    // Fan-out IDs are rooted at '<root>/<index>'. Retain every child until its root has finished,
    // even when a provider budget leaves that root waiting for weeks.
    collection
      .aggregate[JobIdEntity](
        Aggregate
          .matchBy(expired && Filter.regex(Field.id, "^[^/]+$"))
          .matchBy(Filter.nin(Field.id, protectedIds.toList.map(_.value)))
          .project(Projection.include(Field.id))
      )
      .stream
      .evalMap { root =>
        // Keep the root as a recovery marker until its descendants have gone.
        collection.deleteMany(Filter.regex(Field.id, s"^${Pattern.quote(root._id.value)}/")).void >>
          collection.deleteOne(Filter.idEq(root._id.value) && expired).void
      }
      .compile
      .drain
  }
}

object JobRepository extends MongoJsonCodecs {
  val CollectionName = "jobs"

  object Field:
    val id             = "_id"
    val job            = "job"
    val priority       = "priority"
    val state          = "job.state"
    val leaseToken     = "job.leaseToken"
    val leaseUntil     = "job.leaseUntil"
    val availableAt    = "job.availableAt"
    val createdAt      = "job.createdAt"
    val completedAt    = "job.completedAt"
    val error          = "job.error"
    val plan           = "job.plan"
    val children       = "job.children"
    val waitCount      = "job.waitCount"
    val retries        = "job.retries"
    val pausedProvider = "job.pausedProvider"
    val commandId      = "job.command.commandId"
    val commandVersion = "job.command.version"
    val count          = "count"

  val Retention                              = 30.days
  private[repositories] val ClaimIndexFields = List(Field.state, Field.priority, Field.availableAt, Field.createdAt, Field.id)
  // The API codec encodes Instants as JSON strings; Mongo claims and updates use BSON dates.
  private given Codec.AsObject[Job] = Codec.AsObject.derived[Job]

  def make[F[_]: Async](db: MongoDatabase[F]): F[JobRepository[F]] =
    for {
      collection <- db.getCollectionWithCodec[JobEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(ClaimIndexFields))
      _          <- collection.createIndex(Index.ascending(List(Field.state, Field.leaseUntil)))
      _          <- collection.createIndex(Index.ascending(Field.commandId))
      _          <- collection.createIndex(Index.ascending(List(Field.state, Field.completedAt)))
    } yield new LiveJobRepository(
      collection
        .withAddedCodec[Provider]
        .withAddedCodec[Job]
        .withAddedCodec[JobPlan]
        .withAddedCodec[JobStateCount]
        .withAddedCodec[JobIdEntity]
    )
}
