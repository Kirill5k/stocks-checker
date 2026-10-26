package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import com.mongodb.client.model.ReturnDocument
import fs2.Stream
import mongo4cats.collection.MongoCollection
import mongo4cats.models.collection.FindOneAndUpdateOptions
import mongo4cats.operations.{Filter, Index, Update}
import mongo4cats.circe.given
import mongo4cats.database.MongoDatabase
import stockschecker.actions.Action
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Command, CommandId, CreateCommand, Schedule, UpdateCommand}
import stockschecker.domain.{Job, JobCommand, ScheduledOccurrence}
import kirill5k.common.cats.syntax.applicative.*
import io.circe.Codec
import mongo4cats.bson.ObjectId

import java.time.Instant

trait CommandRepository[F[_]]:
  def all: F[List[Command]]
  def streamActive: Stream[F, Command]
  def find(id: CommandId): F[Command]
  def create(cmd: CreateCommand): F[Command]
  def update(cmd: UpdateCommand): F[Command]
  def update(cmd: Command): F[Command]
  def setActive(id: CommandId, isActive: Boolean): F[Unit]
  def initializeNextRun(cmd: Command, now: Instant): F[Unit]
  def reserve(cmd: Command, now: Instant): F[Option[ScheduledOccurrence]]
  def clearOutbox(id: CommandId, runId: String): F[Unit]
  def completeRun(id: CommandId, runId: String): F[Unit]

final private case class ScheduledCommandEntity(
    _id: ObjectId,
    isActive: Boolean,
    action: Action,
    schedule: Schedule,
    lastExecutedAt: Option[Instant],
    executionCount: Int,
    maxExecutions: Option[Int],
    version: Option[Long],
    nextRunAt: Option[Instant],
    activeRunId: Option[String],
    outbox: Option[ScheduledOccurrence]
) derives Codec.AsObject {
  def toDomain: Command = Command(
    CommandId(_id),
    isActive,
    action,
    schedule,
    lastExecutedAt,
    executionCount,
    maxExecutions,
    version,
    nextRunAt,
    activeRunId,
    outbox
  )
}

final private class LiveCommandRepository[F[_]](
    private val collection: MongoCollection[F, ScheduledCommandEntity]
)(using
    F: MonadThrow[F]
) extends CommandRepository[F] {

  import CommandRepository.Field

  override def all: F[List[Command]] =
    collection.find.all.mapList(_.toDomain)

  override def streamActive: Stream[F, Command] =
    collection.find(Filter.eq(Field.isActive, true)).stream.map(_.toDomain)

  override def find(id: CommandId): F[Command] =
    collection
      .find(Filter.idEq(id.toObjectId))
      .first
      .flatMap(toCommandOrError(id))

  override def create(cmd: CreateCommand): F[Command] =
    val newCmd =
      ScheduledCommandEntity(ObjectId.gen, true, cmd.action, cmd.schedule, None, 0, cmd.maxExecutions, Some(0L), None, None, None)
    collection.insertOne(newCmd).as(newCmd.toDomain)

  override def update(cmd: UpdateCommand): F[Command] =
    editCommand(
      cmd.id,
      Update
        .set(Field.isActive, cmd.isActive)
        .set(Field.action, cmd.action)
        .set(Field.schedule, cmd.schedule)
        .set(Field.maxExecutions, cmd.maxExecutions)
    )

  override def update(cmd: Command): F[Command] =
    updateCommand(
      cmd.id,
      Update
        .set(Field.isActive, cmd.isActive)
        .set(Field.action, cmd.action)
        .set(Field.schedule, cmd.schedule)
        .set(Field.lastExecutedAt, cmd.lastExecutedAt)
        .set(Field.executionCount, cmd.executionCount)
        .set(Field.maxExecutions, cmd.maxExecutions)
    )

  private def updateCommand(id: CommandId, update: Update): F[Command] =
    collection
      .findOneAndUpdate(
        Filter.idEq(id.toObjectId),
        update,
        FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
      )
      .flatMap(toCommandOrError(id))

  override def setActive(id: CommandId, isActive: Boolean): F[Unit] =
    editCommand(id, Update.set(Field.isActive, isActive)).void

  private def versionFilter(cmd: Command): Filter =
    Filter.idEq(cmd.id.toObjectId) && cmd.version.fold(Filter.isNull("version"))(v => Filter.eq("version", v))

  private def editCommand(id: CommandId, update: Update): F[Command] =
    find(id).flatMap { before =>
      collection
        .findOneAndUpdate(
          versionFilter(before),
          update
            .set("version", before.revision + 1L)
            .set("nextRunAt", Option.empty[Instant]),
          FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
        )
        .flatMap {
          case Some(entity) => entity.toDomain.pure
          case None         => editCommand(id, update)
        }
    }

  override def initializeNextRun(cmd: Command, now: Instant): F[Unit] =
    if (!cmd.canBeExecuted || cmd.nextRunAt.nonEmpty || cmd.activeRunId.nonEmpty) F.unit
    else
      collection
        .updateOne(
          versionFilter(cmd) && Filter.isNull("nextRunAt") && Filter.isNull("activeRunId"),
          Update.set("nextRunAt", now.plusMillis(cmd.durationUntilNextExecution(now).toMillis))
        )
        .void

  override def reserve(cmd: Command, now: Instant): F[Option[ScheduledOccurrence]] =
    if (!cmd.canBeExecuted || cmd.activeRunId.nonEmpty || !cmd.nextRunAt.exists(!_.isAfter(now))) None.pure
    else {
      val runId      = s"${cmd.id.value}:${cmd.revision}:${cmd.nextRunAt.get.toEpochMilli}"
      val occurrence = ScheduledOccurrence(Job(runId, cmd.action, now, now, Some(JobCommand(cmd.id, cmd.revision, runId))))
      val next       = if (cmd.maxExecutions.exists(_ <= cmd.executionCount + 1)) None else Some(cmd.schedule.nextExecutionTime(now))
      collection
        .findOneAndUpdate(
          versionFilter(cmd) && Filter.eq(Field.isActive, true) && Filter.eq(Field.executionCount, cmd.executionCount) &&
            Filter.isNull("activeRunId") && Filter.isNull("outbox") && Filter.lte("nextRunAt", now),
          Update
            .set("outbox", occurrence)
            .set("activeRunId", runId)
            .set(Field.executionCount, cmd.executionCount + 1)
            .set(Field.lastExecutedAt, now)
            .set("nextRunAt", next),
          FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
        )
        .map(_.flatMap(_.outbox))
    }

  override def clearOutbox(id: CommandId, runId: String): F[Unit] =
    collection
      .updateOne(Filter.idEq(id.toObjectId) && Filter.eq("outbox.job.id", runId), Update.set("outbox", Option.empty[ScheduledOccurrence]))
      .void

  override def completeRun(id: CommandId, runId: String): F[Unit] =
    collection
      .updateOne(Filter.idEq(id.toObjectId) && Filter.eq("activeRunId", runId), Update.set("activeRunId", Option.empty[String]))
      .void

  private def toCommandOrError(id: CommandId)(maybeCmd: Option[ScheduledCommandEntity]): F[Command] =
    F.fromOption(maybeCmd.map(_.toDomain), AppError.EntityDoesNotExist("Command", id.value))

}

object CommandRepository:
  val CollectionName = "commands"
  object Field:
    val isActive       = "isActive"
    val action         = "action"
    val schedule       = "schedule"
    val lastExecutedAt = "lastExecutedAt"
    val executionCount = "executionCount"
    val maxExecutions  = "maxExecutions"

  def make[F[_]](db: MongoDatabase[F])(using F: MonadThrow[F]): F[CommandRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[ScheduledCommandEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.isActive))
      _          <- collection.createIndex(Index.ascending(List(Field.isActive, "nextRunAt")))
    yield LiveCommandRepository(collection.withAddedCodec[Schedule].withAddedCodec[Action].withAddedCodec[ScheduledOccurrence])
