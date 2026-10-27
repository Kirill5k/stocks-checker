package stockschecker.actions

import cats.effect.IO
import cats.syntax.parallel.*
import kirill5k.common.cats.Clock
import stockschecker.domain.*
import stockschecker.fixtures.*
import stockschecker.repositories.{CommandRepository, JobRepository, RepositorySpec}
import stockschecker.services.CommandService

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class CommandSchedulerIntegrationSpec extends RepositorySpec {
  override def port: Int = 12211
  given Clock[IO]        = Clock.make[IO]
  private val action     = Action.DeactivateSecurity(AAPL)
  "A command scheduler" should {
    "recover the command outbox concurrently after a crash without counting the occurrence twice" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs      <- JobRepository.make[IO](db)
        commands  <- CommandRepository.make[IO](db)
        cmd       <- commands.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
        _         <- commands.initializeNextRun(cmd, now)
        due       <- commands.find(cmd.id)
        reserved  <- commands.reserve(due, now)
        before    <- jobs.find(reserved.get.job.id)
        _         <- (CommandScheduler.tick(commands, jobs), CommandScheduler.tick(commands, jobs)).parTupled
        _         <- CommandScheduler.tick(commands, jobs)
        after     <- commands.find(cmd.id)
        claimed   <- jobs.claim(Instant.now(), 2.minutes).map(_.get)
        _         <- jobs.finish(claimed, Job.Succeeded)
        _         <- CommandScheduler.tick(commands, jobs)
        exhausted <- commands.find(cmd.id)
      } yield {
        reserved must not be empty
        before mustBe None
        after.executionCount mustBe 1
        after.outbox mustBe None
        exhausted.activeRunId mustBe None
        exhausted.nextRunAt mustBe None
        exhausted.canBeExecuted mustBe false
      }
    }

    "cancel queued old versions on edits and deactivate, and schedule again on activation" in withEmbeddedMongoDatabase { db =>
      for {
        jobs        <- JobRepository.make[IO](db)
        commands    <- CommandRepository.make[IO](db)
        dispatcher  <- ActionDispatcher.durable(jobs)
        service     <- CommandService.make(commands, dispatcher, jobs)
        cmd         <- service.create(CreateCommand(action, Schedule.Periodic(1.hour), None))
        _           <- CommandScheduler.tick(commands, jobs)
        before      <- commands.find(cmd.id)
        _           <- service.update(UpdateCommand(cmd.id, true, Action.DeactivateSecurity(MSFT), Schedule.Periodic(1.second), None))
        old         <- jobs.find(before.activeRunId.get).map(_.get)
        _           <- service.activate(cmd.id, false)
        _           <- CommandScheduler.tick(commands, jobs)
        inactive    <- commands.find(cmd.id)
        _           <- service.activate(cmd.id, true)
        reactivated <- commands.find(cmd.id)
      } yield {
        old.state mustBe Job.Cancelled
        inactive.isActive mustBe false
        inactive.activeRunId mustBe None
        reactivated.isActive mustBe true
        reactivated.nextRunAt must not be empty
      }
    }

  }
}
