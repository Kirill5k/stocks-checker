package stockschecker.repositories

import cats.{MonadError, MonadThrow}
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import com.mongodb.client.result.UpdateResult
import fs2.Stream
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.{Filter, Update}
import mongo4cats.circe.given
import mongo4cats.database.MongoDatabase
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Command, CommandId, CreateCommand, Schedule}
import stockschecker.repositories.entities.CommandEntity

trait CommandRepository[F[_]]:
  def streamActive: Stream[F, Command]
  def find(id: CommandId): F[Command]
  def create(cmd: CreateCommand): F[Command]
  def update(cmd: Command): F[Command]
  def setActive(id: CommandId, isActive: Boolean): F[Unit]

final private class LiveCommandRepository[F[_]](
    private val collection: MongoCollection[F, CommandEntity]
)(using
    F: MonadThrow[F]
) extends CommandRepository[F] {

  private object Field:
    val isActive       = "isActive"
    val schedule       = "schedule"
    val lastExecutedAt = "lastExecutedAt"
    val executionCount = "executionCount"
    val maxExecutions = "maxExecutions"

  override def streamActive: Stream[F, Command] =
    collection.find(Filter.eq(Field.isActive, true)).stream.map(_.toDomain)

  override def find(id: CommandId): F[Command] =
    collection
      .find(Filter.idEq(id.toObjectId))
      .first
      .flatMap {
        case Some(cmd) => cmd.toDomain.pure[F]
        case None      => AppError.EntityDoesNotExist("Command", id.value).raiseError
      }

  override def create(cmd: CreateCommand): F[Command] =
    val newCmd = CommandEntity.from(cmd)
    collection.insertOne(newCmd).as(newCmd.toDomain)

  override def update(cmd: Command): F[Command] =
    collection
      .updateOne(
        Filter.idEq(cmd.id.toObjectId),
        Update
          .set(Field.isActive, cmd.isActive)
          .set(Field.schedule, cmd.schedule)
          .set(Field.lastExecutedAt, cmd.lastExecutedAt)
          .set(Field.executionCount, cmd.executionCount)
          .set(Field.maxExecutions, cmd.maxExecutions)
      )
      .flatMap(errorIfNoMatches(AppError.EntityDoesNotExist("Command", cmd.id.value)))
      .as(cmd)

  override def setActive(id: CommandId, isActive: Boolean): F[Unit] =
    collection
      .updateOne(Filter.idEq(id.toObjectId), Update.set(Field.isActive, isActive))
      .flatMap(errorIfNoMatches(AppError.EntityDoesNotExist("Command", id.value)))

  private def errorIfNoMatches(error: Throwable)(res: UpdateResult)(using F: MonadError[F, Throwable]): F[Unit] =
    F.raiseWhen(res.getMatchedCount == 0)(error)
}

object CommandRepository:
  def make[F[_]](db: MongoDatabase[F])(using F: MonadThrow[F]): F[CommandRepository[F]] =
    db
      .getCollectionWithCodec[CommandEntity]("commands")
      .map(_.withAddedCodec[Schedule])
      .map(LiveCommandRepository(_))