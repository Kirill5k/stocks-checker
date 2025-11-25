package stockschecker.repositories

import cats.MonadThrow
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import com.mongodb.client.model.ReturnDocument
import com.mongodb.client.result.UpdateResult
import fs2.Stream
import mongo4cats.collection.MongoCollection
import mongo4cats.models.collection.FindOneAndUpdateOptions
import mongo4cats.operations.{Filter, Index, Update}
import mongo4cats.circe.given
import mongo4cats.database.MongoDatabase
import stockschecker.actions.Action
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Command, CommandId, CreateCommand, Schedule, UpdateCommand}
import stockschecker.repositories.entities.CommandEntity
import kirill5k.common.cats.syntax.applicative.*

trait CommandRepository[F[_]]:
  def all: F[List[Command]]
  def streamActive: Stream[F, Command]
  def find(id: CommandId): F[Command]
  def create(cmd: CreateCommand): F[Command]
  def update(cmd: UpdateCommand): F[Command]
  def update(cmd: Command): F[Command]
  def setActive(id: CommandId, isActive: Boolean): F[Unit]

final private class LiveCommandRepository[F[_]](
    private val collection: MongoCollection[F, CommandEntity]
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
    val newCmd = CommandEntity.from(cmd)
    collection.insertOne(newCmd).as(newCmd.toDomain)

  override def update(cmd: UpdateCommand): F[Command] =
    updateCommand(
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
    collection
      .updateOne(Filter.idEq(id.toObjectId), Update.set(Field.isActive, isActive))
      .flatMap(errorIfNoMatches(AppError.EntityDoesNotExist("Command", id.value)))

  private def toCommandOrError(id: CommandId)(maybeCmd: Option[CommandEntity]): F[Command] =
    F.fromOption(maybeCmd.map(_.toDomain), AppError.EntityDoesNotExist("Command", id.value))

  private def errorIfNoMatches(error: Throwable)(res: UpdateResult): F[Unit] =
    F.raiseWhen(res.getMatchedCount == 0)(error)
}

object CommandRepository:

  object Field:
    val isActive       = "isActive"
    val action         = "action"
    val schedule       = "schedule"
    val lastExecutedAt = "lastExecutedAt"
    val executionCount = "executionCount"
    val maxExecutions  = "maxExecutions"
  
  def make[F[_]](db: MongoDatabase[F])(using F: MonadThrow[F]): F[CommandRepository[F]] =
    for
      collection <- db.getCollectionWithCodec[CommandEntity]("commands")
      _          <- collection.createIndex(Index.ascending(Field.isActive))
    yield LiveCommandRepository(collection.withAddedCodec[Schedule].withAddedCodec[Action])
