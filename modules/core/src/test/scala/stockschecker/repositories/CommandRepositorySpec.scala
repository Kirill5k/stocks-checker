package stockschecker.repositories

import cats.data.NonEmptyList
import mongo4cats.operations.Filter
import stockschecker.actions.Action
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CreateCommand, Exchange, Schedule, UpdateCommand}
import stockschecker.fixtures.*

import scala.concurrent.duration.*

class CommandRepositorySpec extends RepositorySpec {

  override def port: Int = 12146

  val action = Action.FetchSecurities(NonEmptyList.of(Exchange.NASDAQ))
  val newCmd = CreateCommand(action, Schedule.Periodic(5.minutes), None)

  "A  CommandRepository" when {
    "create" should {
      "store new command in db" in
        withEmbeddedMongoDatabase { db =>
          for
            repo     <- CommandRepository.make(db)
            cmd      <- repo.create(newCmd)
            foundCmd <- repo.find(cmd.id)
          yield {
            foundCmd mustBe cmd
            cmd.isActive mustBe true
            cmd.lastExecutedAt mustBe None
            cmd.schedule mustBe Schedule.Periodic(5.minutes)
            assert(cmd.action == action)
          }
        }

      "retain BSON schedule timestamps and reserved run IDs after recreation" in
        withEmbeddedMongoDatabase { db =>
          for
            repo       <- CommandRepository.make(db)
            command    <- repo.create(newCmd)
            ready      <- repo.initializeNextRun(command, ts)
            occurrence <- repo.reserve(ready.get, ts)
            before     <- repo.find(command.id)
            raw        <- db.getCollection(CommandRepository.CollectionName)
            stored     <- raw
              .find(
                Filter.idEq(command.id.toObjectId) && Filter.eq("lastExecutedAt", ts) &&
                  Filter.eq("nextRunAt", ts.plusSeconds(5.minutes.toSeconds))
              )
              .first
            recreated <- CommandRepository.make(db)
            after     <- recreated.find(command.id)
          yield
            occurrence must not be empty
            stored must not be empty
            before mustBe occurrence.get
            after mustBe before
        }

    }

    "find" should {
      "return an error when cmd is not found" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            res  <- repo.find(FetchLatestSecuritiesCommand.id).attempt
          yield res mustBe Left(AppError.EntityDoesNotExist("Command", FetchLatestSecuritiesCommand.id.value))
        }
    }

    "update" should {
      "return an error when cmd is not found" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            res  <- repo.update(UpdateCommand(FetchLatestSecuritiesCommand.id, false, action, Schedule.Periodic(5.minutes), None)).attempt
          yield res mustBe Left(AppError.EntityDoesNotExist("Command", FetchLatestSecuritiesCommand.id.value))
        }

      "store updated fields" in
        withEmbeddedMongoDatabase { db =>
          for
            repo       <- CommandRepository.make(db)
            cmd        <- repo.create(newCmd)
            _          <- repo.update(UpdateCommand(cmd.id, false, cmd.action, cmd.schedule, Some(10)))
            updatedCmd <- repo.find(cmd.id)
          yield {
            updatedCmd.isActive mustBe false
            updatedCmd.lastExecutedAt mustBe None
            updatedCmd.executionCount mustBe 0
            updatedCmd.maxExecutions mustBe Some(10)
          }
        }
    }

    "setActive" should {
      "return an error when cmd is not found" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            res  <- repo.setActive(FetchLatestSecuritiesCommand.id, false).attempt
          yield res mustBe Left(AppError.EntityDoesNotExist("Command", FetchLatestSecuritiesCommand.id.value))
        }

      "update isActive field" in
        withEmbeddedMongoDatabase { db =>
          for
            repo       <- CommandRepository.make(db)
            cmd        <- repo.create(newCmd)
            _          <- repo.setActive(cmd.id, false)
            updatedCmd <- repo.find(cmd.id)
          yield updatedCmd.isActive mustBe false
        }
    }

    "pending" should {
      "exclude future and exhausted schedules while including reserved run recovery" in
        withEmbeddedMongoDatabase { db =>
          for {
            repo        <- CommandRepository.make(db)
            future      <- repo.create(CreateCommand(action, Schedule.Cron.unsafe("0 0 * * *"), None))
            _           <- repo.initializeNextRun(future, ts)
            exhausted   <- repo.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
            ready       <- repo.initializeNextRun(exhausted, ts).map(_.get)
            occurrence  <- repo.reserve(ready, ts).map(_.get)
            recoverable <- repo.pending(ts)
            _           <- repo.completeRun(exhausted.id, occurrence.activeRunId.get)
            idle        <- repo.pending(ts)
          } yield {
            recoverable.map(_.id) mustBe List(exhausted.id)
            idle mustBe Nil
          }
        }

      "select uninitialized active commands and exclude inactive ones" in
        withEmbeddedMongoDatabase { db =>
          for
            repo           <- CommandRepository.make(db)
            cmd            <- repo.create(newCmd)
            _              <- repo.setActive(cmd.id, false)
            activeCmd      <- repo.create(newCmd)
            activeCommands <- repo.pending(ts)
          yield activeCommands mustBe List(activeCmd)
        }
    }

    "all" should {
      "return empty list when there are no commands in db" in
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            cmds <- repo.all
          yield cmds mustBe Nil
        }

      "return all commands from repo" in
        withEmbeddedMongoDatabase { db =>
          for
            repo      <- CommandRepository.make(db)
            cmd       <- repo.create(newCmd)
            _         <- repo.setActive(cmd.id, false)
            activeCmd <- repo.create(newCmd)
            cmds      <- repo.all
          yield cmds mustBe List(cmd.copy(isActive = false, version = cmd.version + 1L), activeCmd)
        }
    }
  }

}
