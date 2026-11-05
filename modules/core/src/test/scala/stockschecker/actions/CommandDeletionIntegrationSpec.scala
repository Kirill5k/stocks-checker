package stockschecker.actions

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.foldable.*
import kirill5k.common.cats.Clock
import org.mockito.Mockito.{mock, verify, verifyNoInteractions, when}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.{CommandRepository, JobRepository, RepositorySpec}
import stockschecker.services.{CommandService, CompanyProfileService, Services}

import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class CommandDeletionIntegrationSpec extends RepositorySpec {
  override def port: Int = 12213
  given Clock[IO]        = Clock.make[IO]
  given Logger[IO]       = Slf4jLogger.getLogger[IO]

  private val action = Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL))

  private def enqueue(jobs: JobRepository[IO])(job: Job): IO[Unit] =
    jobs.enqueue(job, ActionPriority.forAction(job.action))

  private def runOnce(executor: ActionExecutor[IO]): IO[Unit] =
    executor.run.take(1).compile.drain.timeout(3.seconds)

  private def awaitTerminal(jobs: JobRepository[IO], id: JobId): IO[Job] =
    jobs
      .find(id)
      .flatMap {
        case Some(job) if job.isTerminal => IO.pure(job)
        case _                           => IO.sleep(30.millis) >> awaitTerminal(jobs, id)
      }
      .timeout(10.seconds)

  "Command deletion" should {
    "delete the command and all scheduled and manual job trees in every state before returning" in withEmbeddedMongoDatabase { db =>
      for
        jobs       <- JobRepository.make[IO](db)
        commands   <- CommandRepository.make[IO](db)
        dispatcher <- ActionDispatcher.make(jobs)
        service    <- CommandService.make(commands, dispatcher)
        command    <- service.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
        _          <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        scheduled  <- commands.find(command.id)
        root       <- jobs.find(scheduled.activeRunId.get).map(_.get)
        manualId   <- service.executeManually(command.id)
        manual     <- jobs.find(manualId).map(_.get)
        now        <- IO.realTimeInstant.map(_.truncatedTo(ChronoUnit.MILLIS))
        unrelated = Job(JobId("unrelated"), action, now.plusSeconds(3600), now)
        children  = JobState.values.toList.zipWithIndex.map { case (state, index) =>
          root.copy(
            id = JobId(s"${root.id.value}/$index"),
            state = state,
            leaseToken = Option.when(state == Job.Running)("in-flight"),
            leaseUntil = Option.when(state == Job.Running)(now.plusSeconds(120))
          )
        }
        _        <- (children ++ List(manual.copy(id = JobId(s"${manual.id.value}/0")), unrelated)).traverse_(enqueue(jobs))
        before   <- jobs.list(100, JobFilter(commandId = Some(command.id)))
        _        <- service.delete(command.id)
        repeated <- service.delete(command.id).attempt
        removed  <- commands.find(command.id).attempt
        linked   <- jobs.list(100, JobFilter(commandId = Some(command.id)))
        retained <- jobs.find(unrelated.id)
        all      <- jobs.list(100, JobFilter())
      yield
        removed.left.toOption.get mustBe AppError.EntityDoesNotExist("Command", command.id.value)
        repeated mustBe Left(AppError.EntityDoesNotExist("Command", command.id.value))
        before.map(_.state).toSet mustBe JobState.values.toSet
        linked mustBe Nil
        retained mustBe Some(unrelated)
        all mustBe List(unrelated)
    }

    "remove a run enqueued from a scheduler reservation after cleanup without executing it" in withEmbeddedMongoDatabase { db =>
      val profiles = mock(classOf[CompanyProfileService[IO]])
      val services = mock(classOf[Services[IO]])
      when(services.companyProfile).thenReturn(profiles)
      for
        jobs       <- JobRepository.make[IO](db)
        commands   <- CommandRepository.make[IO](db)
        dispatcher <- ActionDispatcher.make(jobs)
        service    <- CommandService.make(commands, dispatcher)
        command    <- service.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
        now        <- IO.realTimeInstant
        reserved   <- commands.reserve(command, now).map(_.get)
        _          <- service.delete(command.id)
        handler    <- ActionHandler.make(services)
        worker     <- ActionExecutor.make(jobs, handler, ActionExecutorConfig(1))
        stale = Job(
          reserved.activeRunId.get,
          reserved.action,
          now,
          now,
          command = Some(JobCommand(reserved.id, reserved.version, reserved.activeRunId.get))
        )
        _      <- enqueue(jobs)(stale)
        _      <- runOnce(worker)
        orphan <- jobs.find(stale.id)
      yield
        orphan mustBe None
        verifyNoInteractions(profiles)
        succeed
    }

    "remove descendants materialized from a saved plan after their command has been deleted" in withEmbeddedMongoDatabase { db =>
      val profiles = mock(classOf[CompanyProfileService[IO]])
      val services = mock(classOf[Services[IO]])
      when(services.companyProfile).thenReturn(profiles)
      for
        jobs       <- JobRepository.make[IO](db)
        commands   <- CommandRepository.make[IO](db)
        dispatcher <- ActionDispatcher.make(jobs)
        service    <- CommandService.make(commands, dispatcher)
        command    <- service.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
        _          <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        now        <- IO.realTimeInstant
        parent     <- jobs.claim(now, 2.minutes).map(_.get)
        plan = JobPlan(List(action), sequential = false)
        saved   <- jobs.savePlan(parent, plan)
        _       <- service.delete(command.id)
        handler <- ActionHandler.make(services)
        worker  <- ActionExecutor.make(jobs, handler, ActionExecutorConfig(1))
        child = Job(JobId(s"${parent.id.value}/0"), plan.actions.head, now, now, command = parent.command)
        _      <- enqueue(jobs)(child)
        _      <- jobs.waitForChildren(parent, List(child.id), now)
        _      <- runOnce(worker)
        root   <- jobs.find(parent.id)
        orphan <- jobs.find(child.id)
      yield
        saved mustBe true
        root mustBe None
        orphan mustBe None
        verifyNoInteractions(profiles)
        succeed
    }

    "preserve manual execution of inactive edited commands and carry deletion lineage through nested fan-out" in withEmbeddedMongoDatabase {
      db =>
        val services = mock(classOf[Services[IO]])
        val profiles = mock(classOf[CompanyProfileService[IO]])
        val batch    = Action.Sequence(NonEmptyList.one(Action.UpdateCompanyProfiles(NonEmptyList.of(AAPL, MSFT))))
        when(services.companyProfile).thenReturn(profiles)
        when(profiles.get(AAPL, true)).thenReturn(IO.pure(AAPLCompanyProfile))
        when(profiles.get(MSFT, true)).thenReturn(IO.pure(MSFTCompanyProfile))
        for
          jobs       <- JobRepository.make[IO](db)
          commands   <- CommandRepository.make[IO](db)
          dispatcher <- ActionDispatcher.make(jobs)
          service    <- CommandService.make(commands, dispatcher)
          command    <- service.create(CreateCommand(batch, Schedule.Periodic(1.hour), Some(1)))
          _          <- service.activate(command.id, false)
          manual     <- service.executeManually(command.id)
          _          <- service.activate(command.id, false)
          handler    <- ActionHandler.make(services)
          worker     <- ActionExecutor.make(jobs, handler, ActionExecutorConfig(1))
          result     <- worker.run.compile.drain.background.use(_ => awaitTerminal(jobs, manual))
          tree       <- jobs.list(100, JobFilter(commandId = Some(command.id)))
          _          <- service.delete(command.id)
          removed    <- jobs.list(100, JobFilter(commandId = Some(command.id)))
        yield
          result.state mustBe Job.Succeeded
          tree.size mustBe 4
          tree.map(_.sourceCommandId).toSet mustBe Set(Some(command.id))
          tree.map(_.command).toSet mustBe Set(None)
          removed mustBe Nil
          verify(profiles).get(AAPL, true)
          verify(profiles).get(MSFT, true)
          succeed
    }
  }
}
