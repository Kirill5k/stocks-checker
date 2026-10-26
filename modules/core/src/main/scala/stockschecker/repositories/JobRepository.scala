package stockschecker.repositories

import cats.effect.Async
import cats.syntax.all.*
import com.mongodb.client.model.ReturnDocument
import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.{FindOneAndUpdateOptions, UpdateOptions}
import mongo4cats.operations.{Filter, Index, Update}
import stockschecker.domain.{CommandId, Job, JobPlan}

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.FiniteDuration

trait JobRepository[F[_]] {
  def enqueue(job: Job): F[Unit]
  def find(id: String): F[Option[Job]]
  def list(limit: Int = 100): F[List[Job]]
  def claim(now: Instant, lease: FiniteDuration): F[Option[Job]]
  def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean]
  def finish(job: Job, state: String, error: Option[String] = None): F[Unit]
  def savePlan(job: Job, plan: JobPlan): F[Boolean]
  def waitForChildren(job: Job, children: List[String], now: Instant): F[Unit]
  def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean): F[Unit]
  def pause(job: Job, provider: String, error: String): F[Unit]
  def resumePaused(provider: String, now: Instant): F[Unit]
  def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit]
}

object JobRepository extends MongoJsonCodecs {
  val CollectionName = "jobs"
  // The API codec encodes Instants as JSON strings; Mongo claims and updates use BSON dates.
  private given Codec.AsObject[Job] = Codec.AsObject.derived[Job]
  private final case class Entity(_id: String, job: Job) derives Codec.AsObject

  def make[F[_]: Async](db: MongoDatabase[F]): F[JobRepository[F]] =
    for {
      collection <- db.getCollectionWithCodec[Entity](CollectionName)
      _ <- collection.createIndex(Index.ascending(List("job.state", "job.availableAt")))
      _ <- collection.createIndex(Index.ascending(List("job.state", "job.leaseUntil")))
      _ <- collection.createIndex(Index.ascending("job.command.commandId"))
    } yield new LiveJobRepository(collection.withAddedCodec[Job].withAddedCodec[JobPlan])

  private final class LiveJobRepository[F[_]: Async](collection: MongoCollection[F, Entity]) extends JobRepository[F] {
    private def owned(job: Job): Filter =
      Filter.idEq(job.id) && Filter.eq("job.state", Job.Running) && Filter.eq("job.leaseToken", job.leaseToken)

    private def release: Update = Update.set("job.leaseToken", Option.empty[String]).set("job.leaseUntil", Option.empty[Instant])

    override def enqueue(job: Job): F[Unit] =
      collection.updateOne(Filter.idEq(job.id), Update.setOnInsert("job", job), UpdateOptions(upsert = true)).void

    override def find(id: String): F[Option[Job]] = collection.find(Filter.idEq(id)).first.map(_.map(_.job))

    override def list(limit: Int): F[List[Job]] = collection.find.limit(limit).all.map(_.toList.map(_.job))

    override def claim(now: Instant, lease: FiniteDuration): F[Option[Job]] =
      Async[F].delay(UUID.randomUUID().toString).flatMap { token =>
        val ready = Filter.in("job.state", List(Job.Pending, Job.Waiting)) && Filter.lte("job.availableAt", now)
        val expired = Filter.eq("job.state", Job.Running) && Filter.lte("job.leaseUntil", now)
        collection.findOneAndUpdate(
          ready || expired,
          Update.set("job.state", Job.Running).set("job.leaseToken", token).set("job.leaseUntil", now.plusMillis(lease.toMillis)),
          FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
        ).map(_.map(_.job))
      }

    override def renew(job: Job, now: Instant, lease: FiniteDuration): F[Boolean] =
      collection.updateOne(owned(job), Update.set("job.leaseUntil", now.plusMillis(lease.toMillis))).map(_.getMatchedCount == 1)

    override def finish(job: Job, state: String, error: Option[String]): F[Unit] =
      collection.updateOne(owned(job), release.set("job.state", state).set("job.error", error)).void

    override def savePlan(job: Job, plan: JobPlan): F[Boolean] =
      collection.updateOne(owned(job), Update.set("job.plan", plan)).map(_.getMatchedCount == 1)

    override def waitForChildren(job: Job, children: List[String], now: Instant): F[Unit] =
      collection.updateOne(owned(job), release.set("job.state", Job.Waiting).set("job.children", children).set("job.availableAt", now.plusSeconds(1))).void

    override def defer(job: Job, until: Instant, error: Option[String], countRetry: Boolean): F[Unit] =
      collection.updateOne(owned(job), release.set("job.state", Job.Pending).set("job.availableAt", until)
        .set("job.error", error).set("job.retries", job.retries + (if (countRetry) 1 else 0))).void

    override def pause(job: Job, provider: String, error: String): F[Unit] =
      collection.updateOne(owned(job), release.set("job.state", Job.Paused).set("job.pausedProvider", provider).set("job.error", error)).void

    override def resumePaused(provider: String, now: Instant): F[Unit] =
      collection.updateMany(Filter.eq("job.state", Job.Paused) && Filter.eq("job.pausedProvider", provider),
        Update.set("job.state", Job.Pending).set("job.availableAt", now).set("job.pausedProvider", Option.empty[String])).void

    override def cancelObsolete(commandId: CommandId, currentVersion: Long): F[Unit] =
      collection.updateMany(
        Filter.eq("job.command.commandId", commandId.value) && Filter.lt("job.command.version", currentVersion) &&
          Filter.in("job.state", List(Job.Pending, Job.Waiting, Job.Paused)),
        release.set("job.state", Job.Cancelled).set("job.error", "Command changed or deactivated")
      ).void
  }
}
