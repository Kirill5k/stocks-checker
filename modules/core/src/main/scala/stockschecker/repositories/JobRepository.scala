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
  def pruneCompleted(now: Instant): F[Unit]
}

final private class LiveJobRepository[F[_]](
    collection: MongoCollection[F, JobEntity]
)(using
    F: Async[F]
) extends JobRepository[F] {
  import JobRepository.{ClaimIndexFields, Field, Retention}

  private def owned(job: Job): Filter =
    Filter.idEq(job.id.value) && Filter.eq(Field.State, Job.Running.value) && Filter.eq(Field.LeaseToken, job.leaseToken)

  private def release: Update = Update.set(Field.LeaseToken, Option.empty[String]).set(Field.LeaseUntil, Option.empty[Instant])

  private val terminal = Filter.in(Field.State, Job.terminalStates.map(_.value))

  override def enqueue(job: Job, priority: Int): F[Unit] =
    collection
      .updateOne(
        Filter.idEq(job.id.value),
        Update
          .setOnInsert(Field.Job, job)
          .setOnInsert(Field.Priority, priority),
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
            .matchBy(Filter.in(Field.Id, ids.map(_.value)))
            .group("$" + Field.State, Accumulator.sum(Field.Count, 1))
        )
        .all
        .map(rows => JobStatusSummary(rows.map(row => row._id -> row.count).toMap))

  override def pendingIds(ids: List[JobId]): F[Set[JobId]] =
    if ids.isEmpty then Set.empty[JobId].pure[F]
    else
      collection
        .aggregate[JobIdEntity](
          Aggregate
            .matchBy(Filter.in(Field.Id, ids.map(_.value)) && Filter.nin(Field.State, Job.terminalStates.map(_.value)))
            .project(Projection.include(Field.Id))
        )
        .all
        .map(_.map(_._id).toSet)

  override def claim(now: Instant, lease: FiniteDuration): F[Option[Job]] =
    Async[F].delay(UUID.randomUUID().toString).flatMap { token =>
      val ready   = Filter.in(Field.State, List(Job.Pending.value, Job.Waiting.value)) && Filter.lte(Field.AvailableAt, now)
      val expired = Filter.eq(Field.State, Job.Running.value) && Filter.lte(Field.LeaseUntil, now)
      collection
        .findOneAndUpdate(
          ready || expired,
          Update.set(Field.State, Job.Running.value).set(Field.LeaseToken, token).set(Field.LeaseUntil, now.plusMillis(lease.toMillis)),
          FindOneAndUpdateOptions(
            returnDocument = ReturnDocument.AFTER,
            sort = Some(Sort.asc(Field.Priority, Field.AvailableAt, Field.CreatedAt, Field.Id))
            // Without this hint Mongo may choose the lease index and sort the entire ready backlog.
            // The ordered index merges the three state scans and stops after the first eligible job.
          ).hint(Indexes.ascending(ClaimIndexFields.asJava))
        )
        .map(_.map(_.job))
    }

  override def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean] =
    collection.updateOne(owned(job), Update.set(Field.LeaseUntil, now.plusMillis(lease.toMillis))).map(_.getMatchedCount == 1)

  override def finish(job: Job, state: JobState, error: Option[String]): F[Unit] =
    collection.updateOne(owned(job), release.set(Field.State, state.value).set(Field.Error, error).currentDate(Field.CompletedAt)).void

  override def savePlan(job: Job, plan: JobPlan): F[Boolean] =
    collection.updateOne(owned(job), Update.set(Field.Plan, plan)).map(_.getMatchedCount == 1)

  override def waitForChildren(job: Job, children: List[JobId], availableAt: Instant): F[Unit] =
    collection
      .updateOne(
        owned(job),
        release
          .set(Field.State, Job.Waiting.value)
          .set(Field.Children, children.map(_.value))
          .set(Field.AvailableAt, availableAt)
          .set(Field.WaitCount, job.waitCount + 1)
      )
      .void

  override def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean): F[Unit] =
    collection
      .updateOne(
        owned(job),
        release
          .set(Field.State, Job.Pending.value)
          .set(Field.AvailableAt, until)
          .set(Field.Error, error)
          .set(Field.Retries, job.retries + (if (countRetry) 1 else 0))
          .set(Field.WaitCount, job.waitCount + 1)
      )
      .void

  override def pause(job: Job, provider: Provider, error: String): F[Unit] =
    collection
      .updateOne(owned(job), release.set(Field.State, Job.Paused.value).set(Field.PausedProvider, provider.print).set(Field.Error, error))
      .void

  override def resumePaused(provider: Provider, now: Instant): F[Unit] =
    collection
      .updateMany(
        Filter.eq(Field.State, Job.Paused.value) && Filter.eq(Field.PausedProvider, provider.print),
        Update.set(Field.State, Job.Pending.value).set(Field.AvailableAt, now).set(Field.PausedProvider, Option.empty[Provider])
      )
      .void

  override def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit] =
    collection
      .updateMany(
        Filter.eq(Field.CommandId, commandId.value) && Filter.lt(Field.CommandVersion, currentVersion) &&
          Filter.in(Field.State, List(Job.Pending.value, Job.Waiting.value, Job.Paused.value)),
        release.set(Field.State, Job.Cancelled.value).set(Field.Error, "Command changed or deactivated").currentDate(Field.CompletedAt)
      )
      .void

  override def pruneCompleted(now: Instant): F[Unit] = {
    val expired = terminal && Filter.lte(Field.CompletedAt, now.minusSeconds(Retention.toSeconds))
    // Fan-out IDs are rooted at '<root>/<index>'. Retain every child until its root has finished,
    // even when a provider budget leaves that root waiting for weeks.
    collection
      .aggregate[JobIdEntity](
        Aggregate
          .matchBy(expired && Filter.regex(Field.Id, "^[^/]+$"))
          .project(Projection.include(Field.Id))
      )
      .stream
      .evalMap { root =>
        // Keep the root as a recovery marker until its descendants have gone.
        collection.deleteMany(Filter.regex(Field.Id, s"^${Pattern.quote(root._id.value)}/")).void >>
          collection.deleteOne(Filter.idEq(root._id.value) && expired).void
      }
      .compile
      .drain
  }
}

object JobRepository extends MongoJsonCodecs {
  val CollectionName = "jobs"

  object Field:
    val Id             = "_id"
    val Job            = "job"
    val Priority       = "priority"
    val State          = "job.state"
    val LeaseToken     = "job.leaseToken"
    val LeaseUntil     = "job.leaseUntil"
    val AvailableAt    = "job.availableAt"
    val CreatedAt      = "job.createdAt"
    val CompletedAt    = "job.completedAt"
    val Error          = "job.error"
    val Plan           = "job.plan"
    val Children       = "job.children"
    val WaitCount      = "job.waitCount"
    val Retries        = "job.retries"
    val PausedProvider = "job.pausedProvider"
    val CommandId      = "job.command.commandId"
    val CommandVersion = "job.command.version"
    val Count          = "count"

  val Retention                              = 30.days
  private[repositories] val ClaimIndexFields = List(Field.State, Field.Priority, Field.AvailableAt, Field.CreatedAt, Field.Id)
  // The API codec encodes Instants as JSON strings; Mongo claims and updates use BSON dates.
  private given Codec.AsObject[Job] = Codec.AsObject.derived[Job]

  def make[F[_]: Async](db: MongoDatabase[F]): F[JobRepository[F]] =
    for {
      collection <- db.getCollectionWithCodec[JobEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(ClaimIndexFields))
      _          <- collection.createIndex(Index.ascending(List(Field.State, Field.LeaseUntil)))
      _          <- collection.createIndex(Index.ascending(Field.CommandId))
      _          <- collection.createIndex(Index.ascending(List(Field.State, Field.CompletedAt)))
    } yield new LiveJobRepository(
      collection
        .withAddedCodec[Provider]
        .withAddedCodec[Job]
        .withAddedCodec[JobPlan]
        .withAddedCodec[JobStateCount]
        .withAddedCodec[JobIdEntity]
    )
}
