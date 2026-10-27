package stockschecker.actions

import cats.effect.{IO, Ref}
import cats.syntax.parallel.*
import kirill5k.common.cats.Clock
import org.mockito.ArgumentMatchers.{any, anyInt}
import org.mockito.Mockito.{mock, when}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
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
  given Logger[IO]       = Slf4jLogger.getLogger[IO]
  private val action     = Action.DeactivateSecurity(AAPL)
  "A command scheduler" should {
    "recover a reserved command concurrently after a crash without counting the occurrence twice" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs     <- JobRepository.make[IO](db)
        commands <- CommandRepository.make[IO](db)
        cmd      <- commands.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
        _        <- commands.initializeNextRun(cmd, now)
        due      <- commands.find(cmd.id)
        reserved <- commands.reserve(due, now)
        before   <- jobs.find(reserved.get.activeRunId.get)
        _        <- (CommandScheduler.make(commands, jobs).flatMap(_.tick), CommandScheduler.make(commands, jobs).flatMap(_.tick)).parTupled
        _        <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        after    <- commands.find(cmd.id)
        claimed  <- jobs.claim(Instant.now(), 2.minutes).map(_.get)
        _        <- jobs.finish(claimed, Job.Succeeded)
        _        <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        exhausted <- commands.find(cmd.id)
      } yield {
        reserved must not be empty
        before mustBe None
        after.executionCount mustBe 1
        exhausted.activeRunId mustBe None
        exhausted.nextRunAt mustBe None
        exhausted.canBeExecuted mustBe false
      }
    }

    "cancel queued old versions on edits and deactivate, and schedule again on activation" in withEmbeddedMongoDatabase { db =>
      for {
        jobs        <- JobRepository.make[IO](db)
        commands    <- CommandRepository.make[IO](db)
        dispatcher  <- ActionDispatcher.make(jobs)
        service     <- CommandService.make(commands, dispatcher)
        cmd         <- service.create(CreateCommand(action, Schedule.Periodic(1.hour), None))
        _           <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        before      <- commands.find(cmd.id)
        _           <- service.update(UpdateCommand(cmd.id, true, Action.DeactivateSecurity(MSFT), Schedule.Periodic(1.second), None))
        old         <- jobs.find(before.activeRunId.get).map(_.get)
        _           <- service.activate(cmd.id, false)
        _           <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
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

    "discard a missing reserved run after the command is edited without assigning the new action to its old ID" in withEmbeddedMongoDatabase {
      db =>
        val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
        for
          jobs      <- JobRepository.make[IO](db)
          commands  <- CommandRepository.make[IO](db)
          original  <- commands.create(CreateCommand(action, Schedule.Periodic(1.hour), None))
          due       <- commands.initializeNextRun(original, now).map(_.get)
          reserved  <- commands.reserve(due, now).map(_.get)
          changed   <- commands.update(UpdateCommand(original.id, true, Action.DeactivateSecurity(MSFT), Schedule.Periodic(1.hour), None))
          scheduler <- CommandScheduler.make(commands, jobs)
          _         <- scheduler.tick
          oldJob    <- jobs.find(reserved.activeRunId.get)
          current   <- commands.find(original.id)
        yield
          oldJob mustBe None
          current.action mustBe changed.action
          current.version mustBe changed.version
          current.executionCount mustBe 1
          current.activeRunId mustBe None
          current.nextRunAt must not be empty
    }

    "release a missing reserved run after deactivation without enqueueing it" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for
        jobs         <- JobRepository.make[IO](db)
        commands     <- CommandRepository.make[IO](db)
        original     <- commands.create(CreateCommand(action, Schedule.Periodic(1.hour), None))
        due          <- commands.initializeNextRun(original, now).map(_.get)
        reserved     <- commands.reserve(due, now).map(_.get)
        _            <- commands.setActive(original.id, false)
        scheduler    <- CommandScheduler.make(commands, jobs)
        _            <- scheduler.tick
        oldJob       <- jobs.find(reserved.activeRunId.get)
        inactive     <- commands.find(original.id)
        protectedIds <- commands.activeRunIds
      yield
        oldJob mustBe None
        inactive.isActive mustBe false
        inactive.activeRunId mustBe None
        protectedIds mustBe empty
    }

    "retain a failed enqueue for recovery while allowing other commands to schedule" in withEmbeddedMongoDatabase { db =>
      val jobs = mock(classOf[JobRepository[IO]])
      for
        accepted <- Ref.of[IO, List[Job]](Nil)
        _ = when(jobs.enqueue(any[Job](), anyInt())).thenAnswer(invocation =>
          val job = invocation.getArgument[Job](0)
          if job.action == action then IO.raiseError[Unit](new IllegalStateException("enqueue failed"))
          else accepted.update(_ :+ job)
        )
        commands  <- CommandRepository.make[IO](db)
        failing   <- commands.create(CreateCommand(action, Schedule.Periodic(1.hour), None))
        healthy   <- commands.create(CreateCommand(Action.DeactivateSecurity(MSFT), Schedule.Periodic(1.hour), None))
        scheduler <- CommandScheduler.make(commands, jobs)
        _         <- scheduler.tick
        persisted <- commands.find(failing.id)
        scheduled <- commands.find(healthy.id)
        delivered <- accepted.get
      yield
        persisted.activeRunId must not be empty
        persisted.executionCount mustBe 1
        scheduled.executionCount mustBe 1
        delivered.map(_.id) mustBe scheduled.activeRunId.toList
        delivered.map(_.action) mustBe List(healthy.action)
    }

  }
}
