package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import com.mongodb.client.model.ReturnDocument
import mongo4cats.collection.MongoCollection
import mongo4cats.models.collection.FindOneAndUpdateOptions
import mongo4cats.operations.{Aggregate, Filter, Index, Projection, Update}
import mongo4cats.circe.given
import mongo4cats.database.MongoDatabase
import stockschecker.actions.Action
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Command, CommandId, CreateCommand, Schedule, UpdateCommand}
import stockschecker.domain.JobId
import kirill5k.common.cats.syntax.applicative.*
import stockschecker.repositories.entities.{CommandActiveRunReference, CommandEntity}

import java.time.Instant

trait CommandRepository[F[_]]:
  def all: F[List[Command]]
  def activeRunIds: F[Set[JobId]]
  def pending(now: Instant): F[List[Command]]
  def find(id: CommandId): F[Command]
  def create(cmd: CreateCommand): F[Command]
  def update(cmd: UpdateCommand): F[Command]
  def setActive(id: CommandId, isActive: Boolean): F[Command]
  def initializeNextRun(cmd: Command, now: Instant): F[Option[Command]]
  def reserve(cmd: Command, now: Instant): F[Option[Command]]
  def completeRun(id: CommandId, runId: JobId): F[Option[Command]]

final private class LiveCommandRepository[F[_]](
    private val collection: MongoCollection[F, CommandEntity]
)(using
    F: MonadThrow[F]
) extends CommandRepository[F] {

  import CommandRepository.Field

  override def all: F[List[Command]] =
    collection.find.all.mapList(_.toDomain)

  override def activeRunIds: F[Set[JobId]] =
    collection
      .aggregate[CommandActiveRunReference](
        Aggregate.matchBy(Filter.ne(Field.activeRunId, Option.empty[String])).project(Projection.include(Field.activeRunId))
      )
      .all
      .map(_.map(_.activeRunId).toSet)

  override def pending(now: Instant): F[List[Command]] = {
    val remaining = Filter.isNull(Field.maxExecutions) ||
      Filter.expr(org.bson.Document.parse(s"""{"$$lt":["$$${Field.executionCount}","$$${Field.maxExecutions}"]}"""))
    val due = Filter.eq(Field.isActive, true) && remaining && Filter.isNull(Field.activeRunId) &&
      (Filter.isNull(Field.nextRunAt) || Filter.lte(Field.nextRunAt, now))
    collection
      .find(due || Filter.ne(Field.activeRunId, Option.empty[String]))
      .all
      .mapList(_.toDomain)
  }

  override def find(id: CommandId): F[Command] =
    collection
      .find(Filter.idEq(id.toObjectId))
      .first
      .flatMap(toCommandOrError(id))

  override def create(cmd: CreateCommand): F[Command] =
    val newCmd = CommandEntity.from(cmd)
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

  override def setActive(id: CommandId, isActive: Boolean): F[Command] =
    editCommand(id, Update.set(Field.isActive, isActive))

  private def versionFilter(cmd: Command): Filter =
    Filter.idEq(cmd.id.toObjectId) && Filter.eq(Field.version, cmd.version)

  private def editCommand(id: CommandId, update: Update): F[Command] =
    find(id).flatMap { before =>
      collection
        .findOneAndUpdate(
          versionFilter(before),
          update
            .set(Field.version, before.version + 1L)
            .set(Field.nextRunAt, Option.empty[Instant]),
          FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
        )
        .flatMap {
          case Some(entity) => entity.toDomain.pure
          case None         => editCommand(id, update)
        }
    }

  override def initializeNextRun(cmd: Command, now: Instant): F[Option[Command]] =
    if (!cmd.canBeExecuted || cmd.nextRunAt.nonEmpty || cmd.activeRunId.nonEmpty) Some(cmd).pure[F]
    else
      collection
        .findOneAndUpdate(
          versionFilter(cmd) && Filter.isNull(Field.nextRunAt) && Filter.isNull(Field.activeRunId),
          Update.set(Field.nextRunAt, now.plusMillis(cmd.durationUntilNextExecution(now).toMillis)),
          FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
        )
        .map(_.map(_.toDomain))

  override def reserve(cmd: Command, now: Instant): F[Option[Command]] =
    if (!cmd.canBeExecuted || cmd.activeRunId.nonEmpty || !cmd.nextRunAt.exists(!_.isAfter(now))) None.pure
    else {
      val runId = JobId(s"${cmd.id.value}:${cmd.version}:${cmd.nextRunAt.get.toEpochMilli}")
      val next  = if (cmd.maxExecutions.exists(_ <= cmd.executionCount + 1)) None else Some(cmd.schedule.nextExecutionTime(now))
      collection
        .findOneAndUpdate(
          versionFilter(cmd) && Filter.eq(Field.isActive, true) && Filter.eq(Field.executionCount, cmd.executionCount) &&
            Filter.isNull(Field.activeRunId) && Filter.lte(Field.nextRunAt, now),
          Update
            .set(Field.activeRunId, runId.value)
            .set(Field.executionCount, cmd.executionCount + 1)
            .set(Field.lastExecutedAt, now)
            .set(Field.nextRunAt, next),
          FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
        )
        .map(_.map(_.toDomain))
    }

  override def completeRun(id: CommandId, runId: JobId): F[Option[Command]] =
    collection
      .findOneAndUpdate(
        Filter.idEq(id.toObjectId) && Filter.eq(Field.activeRunId, runId.value),
        Update.set(Field.activeRunId, Option.empty[String]),
        FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
      )
      .map(_.map(_.toDomain))

  private def toCommandOrError(id: CommandId)(maybeCmd: Option[CommandEntity]): F[Command] =
    F.fromOption(maybeCmd.map(_.toDomain), AppError.EntityDoesNotExist("Command", id.value))

}

object CommandRepository:
  val CollectionName = "commands"
  object Field:
    val version        = "version"
    val nextRunAt      = "nextRunAt"
    val activeRunId    = "activeRunId"
    val isActive       = "isActive"
    val action         = "action"
    val schedule       = "schedule"
    val lastExecutedAt = "lastExecutedAt"
    val executionCount = "executionCount"
    val maxExecutions  = "maxExecutions"

  def make[F[_]](db: MongoDatabase[F])(using F: MonadThrow[F]): F[CommandRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[CommandEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.activeRunId))
      _          <- collection.createIndex(Index.ascending(Field.isActive))
      _          <- collection.createIndex(Index.ascending(List(Field.isActive, Field.nextRunAt)))
    yield LiveCommandRepository(
      collection
        .withAddedCodec[CommandActiveRunReference]
        .withAddedCodec[Schedule]
        .withAddedCodec[Action]
    )
