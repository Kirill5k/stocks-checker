package stockschecker.repositories

import stockschecker.domain.Provider
import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import com.mongodb.client.model.{Indexes, ReturnDocument}
import io.circe.Codec
import kirill5k.common.cats.Clock
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{FindOneAndUpdateOptions, UpdateOptions}
import mongo4cats.operations.{Accumulator, Aggregate, Filter, Index, Projection, Sort, Update}
import stockschecker.domain.{CommandId, Job, JobFilter, JobId, JobPlan, JobState, JobStatusSummary}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.entities.{JobCommandStateEntity, JobEntity, JobIdEntity, JobStateCount}

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

trait JobRepository[F[_]] {
  def enqueue(job: Job, priority: Int): F[Unit]
  def get(id: JobId): F[Job]
  def find(id: JobId): F[Option[Job]]
  def list(limit: Int, filter: JobFilter): F[List[Job]]
  def summarize(ids: List[JobId]): F[JobStatusSummary]
  def pendingIds(ids: List[JobId]): F[Set[JobId]]
  def claim(now: Instant, lease: FiniteDuration): F[Option[Job]]
  def isCurrent(job: Job): F[Boolean]
  def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean]
  def finish(job: Job, state: JobState, error: Option[String] = None): F[Unit]
  def savePlan(job: Job, plan: JobPlan): F[Boolean]
  def waitForChildren(job: Job, children: List[JobId], availableAt: Instant): F[Unit]
  def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean, countDeferral: Boolean = false): F[Unit]
  def pause(job: Job, provider: Provider, error: String): F[Unit]
  def resumePaused(provider: Provider, now: Instant): F[Unit]
  def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit]
  def deleteForCommand(commandId: CommandId): F[Unit]
  def pruneCompleted(now: Instant): F[Unit]
}

final private class LiveJobRepository[F[_]](
    collection: MongoCollection[F, JobEntity]
)(using
    F: Async[F],
    clock: Clock[F]
) extends JobRepository[F] {
  import JobRepository.{ClaimIndexFields, Field, MaxLeaseReclaims, Retention}

  private def owned(job: Job): Filter =
    Filter.idEq(job.id.value) &&
      Filter.eq(Field.State, Job.Running.value) &&
      Filter.eq(Field.LeaseToken, job.leaseToken)

  private def release: Update =
    Update
      .set(Field.LeaseToken, Option.empty[String])
      .set(Field.LeaseUntil, Option.empty[Instant])

  private val terminal =
    Filter.in(Field.State, Job.terminalStates.map(_.value))

  private def forCommand(commandId: CommandId): Filter =
    Filter.eq(Field.CommandId, commandId.value) || Filter.eq(Field.SourceCommandId, commandId.value)

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

  override def get(id: JobId): F[Job] =
    find(id).flatMap(job => F.fromOption(job, AppError.EntityDoesNotExist("Job", id.value)))

  override def find(id: JobId): F[Option[Job]] =
    collection
      .find(Filter.idEq(id.value))
      .first
      .mapOpt(_.job)

  override def isCurrent(job: Job): F[Boolean] =
    job.commandId.fold(true.pure[F]) { id =>
      collection
        .aggregate[JobCommandStateEntity](
          Aggregate
            .matchBy(Filter.idEq(job.id.value))
            .lookup(
              CommandRepository.CollectionName,
              Aggregate
                .matchBy(Filter.idEq(id.toObjectId))
                .project(Projection.include(List(CommandRepository.Field.IsActive, CommandRepository.Field.Version)).excludeId),
              Field.Commands
            )
            .project(Projection.include(Field.Commands).excludeId)
        )
        .first
        .flatMap {
          case None      => false.pure[F]
          case Some(row) =>
            F.fromOption(row.commands.headOption, AppError.EntityDoesNotExist("Command", id.value))
              .map(command => job.command.forall(reference => command.isActive && command.version == reference.version))
        }
    }

  override def list(limit: Int, filter: JobFilter): F[List[Job]] =
    val query = List(
      filter.commandId.map(forCommand),
      filter.state.map(state => Filter.eq(Field.State, state.value)),
      filter.actionKind.map(kind => Filter.eq(Field.ActionKind, kind))
    ).flatten.foldLeft(Filter.empty)(_ && _)
    collection
      .find(query)
      .sort(Sort.desc(Field.CreatedAt, Field.Id))
      .limit(limit)
      .all
      .mapList(_.job)

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
    F.delay(UUID.randomUUID().toString).flatMap { token =>
      for
        ready   = Filter.in(Field.State, List(Job.Pending.value, Job.Waiting.value, Job.Paused.value)) && Filter.lte(Field.AvailableAt, now)
        expired = Filter.eq(Field.State, Job.Running.value) && Filter.lte(Field.LeaseUntil, now)
        acquire = Update
          .set(Field.State, Job.Running.value)
          .set(Field.LeaseToken, token)
          .set(Field.LeaseUntil, now.plusMillis(lease.toMillis))
          .set(Field.PausedProvider, Option.empty[Provider])
        options = FindOneAndUpdateOptions(
          returnDocument = ReturnDocument.AFTER,
          sort = Some(Sort.asc(Field.Priority, Field.AvailableAt, Field.CreatedAt, Field.Id))
        ).hint(Indexes.ascending(ClaimIndexFields.asJava))
        // A worker crash consumes a bounded reclaim, independently of normal dependency polling.
        _ <- collection.updateMany(
          expired && Filter.gte(Field.ReclaimCount, MaxLeaseReclaims),
          release.set(Field.State, Job.Failed.value).set(Field.Error, "Lease recovery limit exceeded").set(Field.CompletedAt, now)
        )
        recovered <- collection.findOneAndUpdate(expired, acquire.inc(Field.ReclaimCount, 1), options)
        claimed   <- recovered.fold(collection.findOneAndUpdate(ready, acquire, options))(job => F.pure(Some(job)))
      yield claimed.map(_.job)
    }

  override def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean] =
    collection
      .updateOne(owned(job) && Filter.gt(Field.LeaseUntil, now), Update.set(Field.LeaseUntil, now.plusMillis(lease.toMillis)))
      .map(_.getMatchedCount == 1)

  override def finish(job: Job, state: JobState, error: Option[String]): F[Unit] =
    clock.now
      .flatMap { now =>
        collection.updateOne(owned(job), release.set(Field.State, state.value).set(Field.Error, error).set(Field.CompletedAt, now)).void
      }

  override def savePlan(job: Job, plan: JobPlan): F[Boolean] =
    collection
      .updateOne(owned(job), Update.set(Field.Plan, plan))
      .map(_.getMatchedCount == 1)

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

  override def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean, countDeferral: Boolean): F[Unit] =
    collection
      .updateOne(
        owned(job),
        release
          .set(Field.State, Job.Pending.value)
          .set(Field.AvailableAt, until)
          .set(Field.Error, error)
          .set(Field.Retries, job.retries + (if (countRetry) 1 else 0))
          .set(Field.DeferralCount, job.deferralCount + (if (countDeferral) 1 else 0))
          .set(Field.WaitCount, job.waitCount + (if (!countRetry && !countDeferral) 1 else 0))
      )
      .void

  override def pause(job: Job, provider: Provider, error: String): F[Unit] =
    clock.now.flatMap { now =>
      collection
        .updateOne(
          owned(job),
          release
            .set(Field.State, Job.Paused.value)
            .set(Field.PausedProvider, provider.print)
            .set(Field.Error, error)
            .set(Field.AvailableAt, now.plusMillis(Provider.configurationRetryDelay.toMillis))
        )
        .void
    }

  override def resumePaused(provider: Provider, now: Instant): F[Unit] =
    collection
      .updateMany(
        Filter.eq(Field.State, Job.Paused.value) && Filter.eq(Field.PausedProvider, provider.print),
        Update.set(Field.State, Job.Pending.value).set(Field.AvailableAt, now).set(Field.PausedProvider, Option.empty[Provider])
      )
      .void

  override def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit] =
    clock.now.flatMap { now =>
      collection
        .updateMany(
          Filter.eq(Field.CommandId, commandId.value) && Filter.lt(Field.CommandVersion, currentVersion) &&
            Filter.in(Field.State, List(Job.Pending.value, Job.Waiting.value, Job.Paused.value)),
          release.set(Field.State, Job.Cancelled.value).set(Field.Error, "Command changed or deactivated").set(Field.CompletedAt, now)
        )
        .void
    }

  override def deleteForCommand(commandId: CommandId): F[Unit] =
    collection.deleteMany(forCommand(commandId)).void

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
        collection.deleteMany(Filter.gte(Field.Id, s"${root._id.value}/") && Filter.lt(Field.Id, s"${root._id.value}0")).void >>
          collection.deleteOne(Filter.idEq(root._id.value) && expired).void
      }
      .compile
      .drain
  }
}

object JobRepository extends MongoJsonCodecs {
  val CollectionName = "jobs"

  object Field:
    val Id              = "_id"
    val Job             = "job"
    val Priority        = "priority"
    val State           = "job.state"
    val ActionKind      = "job.action.kind"
    val LeaseToken      = "job.leaseToken"
    val LeaseUntil      = "job.leaseUntil"
    val AvailableAt     = "job.availableAt"
    val CreatedAt       = "job.createdAt"
    val CompletedAt     = "job.completedAt"
    val Error           = "job.error"
    val Plan            = "job.plan"
    val Children        = "job.children"
    val WaitCount       = "job.waitCount"
    val Retries         = "job.retries"
    val DeferralCount   = "job.deferralCount"
    val ReclaimCount    = "job.reclaimCount"
    val PausedProvider  = "job.pausedProvider"
    val CommandId       = "job.command.commandId"
    val SourceCommandId = "job.sourceCommandId"
    val CommandVersion  = "job.command.version"
    val Commands        = "commands"
    val Count           = "count"

  val Retention                              = 30.days
  val MaxLeaseReclaims                       = 3
  private[repositories] val ClaimIndexFields = List(Field.State, Field.Priority, Field.AvailableAt, Field.CreatedAt, Field.Id)
  // The API codec encodes Instants as JSON strings; Mongo claims and updates use BSON dates.
  private given Codec.AsObject[Job] = Codec.AsObject.derived[Job]

  def make[F[_]: {Async, Clock}](db: MongoDatabase[F]): F[JobRepository[F]] =
    for {
      collection <- db.getCollectionWithCodec[JobEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(ClaimIndexFields))
      _          <- collection.createIndex(Index.ascending(Field.CommandId))
      _          <- collection.createIndex(Index.ascending(Field.SourceCommandId))
      _          <- collection.createIndex(Index.ascending(List(Field.State, Field.CompletedAt)))
      _          <- collection.createIndex(Index.descending(List(Field.CreatedAt, Field.Id)))
    } yield new LiveJobRepository(
      collection
        .withAddedCodec[Provider]
        .withAddedCodec[Job]
        .withAddedCodec[JobPlan]
        .withAddedCodec[JobStateCount]
        .withAddedCodec[JobIdEntity]
        .withAddedCodec[JobCommandStateEntity]
    )
}
