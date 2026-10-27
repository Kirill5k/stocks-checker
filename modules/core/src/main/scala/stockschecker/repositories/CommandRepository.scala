package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import com.mongodb.client.model.ReturnDocument
import mongo4cats.collection.MongoCollection
import mongo4cats.models.collection.FindOneAndUpdateOptions
import mongo4cats.operations.{Filter, Index, Update}
import mongo4cats.circe.given
import mongo4cats.database.MongoDatabase
import stockschecker.actions.Action
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Command, CommandId, CreateCommand, Schedule, UpdateCommand}
import stockschecker.domain.JobId
import kirill5k.common.cats.syntax.applicative.*
import stockschecker.repositories.entities.CommandEntity

import java.time.Instant

trait CommandRepository[F[_]]:
  def all: F[List[Command]]
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

  override def pending(now: Instant): F[List[Command]] = {
    val remaining = Filter.isNull(Field.MaxExecutions) ||
      Filter.expr(org.bson.Document.parse(s"""{"$$lt":["$$${Field.ExecutionCount}","$$${Field.MaxExecutions}"]}"""))
    val due = Filter.eq(Field.IsActive, true) && remaining && Filter.isNull(Field.ActiveRunId) &&
      (Filter.isNull(Field.NextRunAt) || Filter.lte(Field.NextRunAt, now))
    collection
      .find(due || Filter.ne(Field.ActiveRunId, Option.empty[String]))
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
        .set(Field.IsActive, cmd.isActive)
        .set(Field.Action, cmd.action)
        .set(Field.Schedule, cmd.schedule)
        .set(Field.MaxExecutions, cmd.maxExecutions)
    )

  override def setActive(id: CommandId, isActive: Boolean): F[Command] =
    editCommand(id, Update.set(Field.IsActive, isActive))

  private def versionFilter(cmd: Command): Filter =
    Filter.idEq(cmd.id.toObjectId) && Filter.eq(Field.Version, cmd.version)

  private def editCommand(id: CommandId, update: Update): F[Command] =
    find(id).flatMap { before =>
      collection
        .findOneAndUpdate(
          versionFilter(before),
          update
            .set(Field.Version, before.version + 1L)
            .set(Field.NextRunAt, Option.empty[Instant]),
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
          versionFilter(cmd) && Filter.isNull(Field.NextRunAt) && Filter.isNull(Field.ActiveRunId),
          Update.set(Field.NextRunAt, now.plusMillis(cmd.durationUntilNextExecution(now).toMillis)),
          FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
        )
        .map(_.map(_.toDomain))

  override def reserve(cmd: Command, now: Instant): F[Option[Command]] =
    cmd.nextRunAt.filter(at => cmd.canBeExecuted && cmd.activeRunId.isEmpty && !at.isAfter(now)) match
      case None     => None.pure[F]
      case Some(at) =>
        val next = if (cmd.maxExecutions.exists(_ <= cmd.executionCount + 1)) None else Some(cmd.schedule.nextExecutionTime(now))
        collection
          .findOneAndUpdate(
            versionFilter(cmd) &&
              Filter.eq(Field.IsActive, true) &&
              Filter.eq(Field.ExecutionCount, cmd.executionCount) &&
              Filter.isNull(Field.ActiveRunId) && Filter.lte(Field.NextRunAt, now),
            Update
              .set(Field.ActiveRunId, cmd.runId(at).value)
              .set(Field.ExecutionCount, cmd.executionCount + 1)
              .set(Field.LastExecutedAt, now)
              .set(Field.NextRunAt, next),
            FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
          )
          .map(_.map(_.toDomain))

  override def completeRun(id: CommandId, runId: JobId): F[Option[Command]] =
    collection
      .findOneAndUpdate(
        Filter.idEq(id.toObjectId) && Filter.eq(Field.ActiveRunId, runId.value),
        Update.set(Field.ActiveRunId, Option.empty[String]),
        FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)
      )
      .map(_.map(_.toDomain))

  private def toCommandOrError(id: CommandId)(maybeCmd: Option[CommandEntity]): F[Command] =
    F.fromOption(maybeCmd.map(_.toDomain), AppError.EntityDoesNotExist("Command", id.value))

}

object CommandRepository:
  val CollectionName = "commands"
  object Field:
    val Version        = "version"
    val NextRunAt      = "nextRunAt"
    val ActiveRunId    = "activeRunId"
    val IsActive       = "isActive"
    val Action         = "action"
    val Schedule       = "schedule"
    val LastExecutedAt = "lastExecutedAt"
    val ExecutionCount = "executionCount"
    val MaxExecutions  = "maxExecutions"

  def make[F[_]](db: MongoDatabase[F])(using F: MonadThrow[F]): F[CommandRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[CommandEntity](CollectionName)
      _          <- collection.createIndex(Index.ascending(Field.ActiveRunId))
      _          <- collection.createIndex(Index.ascending(Field.IsActive))
      _          <- collection.createIndex(Index.ascending(List(Field.IsActive, Field.NextRunAt)))
    yield LiveCommandRepository(
      collection
        .withAddedCodec[Schedule]
        .withAddedCodec[Action]
    )
