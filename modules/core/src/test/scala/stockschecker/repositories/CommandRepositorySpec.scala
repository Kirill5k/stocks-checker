package stockschecker.repositories

import org.scalatest.wordspec.AsyncWordSpec
import stockschecker.actions.Action
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CreateCommand, Schedule}
import stockschecker.fixtures.*

import scala.concurrent.duration.*

class CommandRepositorySpec extends RepositorySpec {

  override def port: Int = 12146

  val newCmd = CreateCommand(Action.FetchLatestStocks, Schedule.Periodic(5.minutes), None)

  "A  CommandRepository" when {
    "create" should {
      "store new command in db" in {
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
            cmd.action mustBe Action.FetchLatestStocks
          }
        }
      }
    }

    "find" should {
      "return an error when cmd is not found" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            res  <- repo.find(FetchLatestStocksCommand.id).attempt
          yield res mustBe Left(AppError.EntityDoesNotExist("Command", FetchLatestStocksCommand.id.value))
        }
      }
    }

    "update" should {
      "return an error when cmd is not found" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            res  <- repo.update(FetchLatestStocksCommand).attempt
          yield res mustBe Left(AppError.EntityDoesNotExist("Command", FetchLatestStocksCommand.id.value))
        }
      }

      "store updated fields" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo       <- CommandRepository.make(db)
            cmd        <- repo.create(newCmd)
            _          <- repo.update(cmd.copy(isActive = false, lastExecutedAt = Some(ts), executionCount = 10, maxExecutions = Some(10)))
            updatedCmd <- repo.find(cmd.id)
          yield {
            updatedCmd.isActive mustBe false
            updatedCmd.lastExecutedAt mustBe Some(ts)
            updatedCmd.executionCount mustBe 10
            updatedCmd.maxExecutions mustBe Some(10)
          }
        }
      }
    }

    "setActive" should {
      "return an error when cmd is not found" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            res  <- repo.setActive(FetchLatestStocksCommand.id, false).attempt
          yield res mustBe Left(AppError.EntityDoesNotExist("Command", FetchLatestStocksCommand.id.value))
        }
      }

      "update isActive field" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo       <- CommandRepository.make(db)
            cmd        <- repo.create(newCmd)
            _          <- repo.setActive(cmd.id, false)
            updatedCmd <- repo.find(cmd.id)
          yield updatedCmd.isActive mustBe false
        }
      }
    }

    "streamActive" should {
      "stream active commands" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo           <- CommandRepository.make(db)
            cmd            <- repo.create(newCmd)
            _              <- repo.setActive(cmd.id, false)
            activeCmd      <- repo.create(newCmd)
            activeCommands <- repo.streamActive.compile.toList
          yield activeCommands mustBe List(activeCmd)
        }
      }
    }

    "all" should {
      "return empty list when there are no commands in db" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo <- CommandRepository.make(db)
            cmds <- repo.all
          yield cmds mustBe Nil
        }
      }

      "return all commands from repo" in {
        withEmbeddedMongoDatabase { db =>
          for
            repo           <- CommandRepository.make(db)
            cmd            <- repo.create(newCmd)
            _              <- repo.setActive(cmd.id, false)
            activeCmd      <- repo.create(newCmd)
            cmds <- repo.all
          yield cmds mustBe List(cmd.copy(isActive = false), activeCmd)
        }
      }
    }
  }

}
