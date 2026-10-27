package stockschecker.actions

import cats.data.NonEmptyList
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.foldable.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import org.mockito.Mockito.{mock, when}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.{CommandRepository, JobRepository, RepositorySpec}
import stockschecker.services.{
  CommandService,
  CompanyProfileService,
  InvestmentEvaluationService,
  InvestmentScreenService,
  SecurityService,
  Services
}

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class ActionExecutorIntegrationSpec extends RepositorySpec {
  override def port: Int                                                 = 12210
  given Clock[IO]                                                        = Clock.make[IO]
  given Logger[IO]                                                       = Slf4jLogger.getLogger[IO]
  private val action                                                     = Action.DeactivateSecurity(AAPL)
  private def enqueue(jobs: JobRepository[IO])(job: Job): IO[Unit]       = jobs.enqueue(job, ActionPriority.forAction(job.action))
  private def awaitTerminal(repo: JobRepository[IO], id: JobId): IO[Job] =
    repo
      .find(id)
      .flatMap {
        case Some(job) if job.isTerminal => IO.pure(job)
        case _                           => IO.sleep(30.millis) >> awaitTerminal(repo, id)
      }
      .timeout(10.seconds)

  private def makeExecutor(jobs: JobRepository[IO], services: Services[IO], config: ActionExecutorConfig): IO[ActionExecutor[IO]] =
    ActionHandler
      .make(services)
      .flatMap(handler =>
        ActionExecutor.make(jobs, handler, command => services.command.isCurrent(command), IO.pure(Set.empty[JobId]), config)
      )

  "An action executor" should {
    "run ready work with sixteen future schedules and no free timer workers" in withEmbeddedMongoDatabase { db =>
      val now      = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      val services = mock(classOf[Services[IO]])
      val security = mock(classOf[SecurityService[IO]])
      when(services.security).thenReturn(security)
      when(security.deactivate(AAPL)).thenReturn(IO.unit)
      for {
        jobs     <- JobRepository.make[IO](db)
        _        <- (1 to 16).toList.traverse_(n => enqueue(jobs)(Job(JobId(s"timer-$n"), action, now.plusSeconds(86400), now)))
        _        <- enqueue(jobs)(Job(JobId("ready"), action, now, now))
        executor <- makeExecutor(jobs, services, ActionExecutorConfig(16))
        _        <- executor.run.take(1).compile.drain.timeout(3.seconds)
        ready    <- jobs.find(JobId("ready")).map(_.get)
        timers   <- jobs.summarize((1 to 16).toList.map(n => JobId(s"timer-$n")))
      } yield {
        ready.state mustBe Job.Succeeded
        timers.count(Job.Pending) mustBe 16
      }
    }

    "execute scheduled investment research and release its completed occurrence" in withEmbeddedMongoDatabase { db =>
      val services    = mock(classOf[Services[IO]])
      val screens     = mock(classOf[InvestmentScreenService[IO]])
      val evaluations = mock(classOf[InvestmentEvaluationService[IO]])
      when(services.investmentScreens).thenReturn(screens)
      when(services.investmentEvaluations).thenReturn(evaluations)
      when(evaluations.captureMonthlyEvaluations).thenReturn(IO.pure(0))
      for {
        runs <- Ref.of[IO, Int](0)
        _ = when(screens.generateScheduledScreen).thenReturn(runs.update(_ + 1).as(None))
        jobs           <- JobRepository.make[IO](db)
        commands       <- CommandRepository.make[IO](db)
        dispatcher     <- ActionDispatcher.make(jobs)
        commandService <- CommandService.make(commands, dispatcher)
        _ = when(services.command).thenReturn(commandService)
        command <- commandService.create(CreateCommand(Action.RefreshInvestmentResearch, Schedule.Periodic(1.hour), Some(1)))
        _       <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        queued  <- commands.find(command.id)
        runId = queued.activeRunId.get
        executor  <- makeExecutor(jobs, services, ActionExecutorConfig(1))
        _         <- executor.run.take(1).compile.drain.timeout(3.seconds)
        result    <- jobs.find(runId).map(_.get)
        count     <- runs.get
        _         <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        completed <- commands.find(command.id)
      } yield {
        count mustBe 1
        result.action mustBe Action.RefreshInvestmentResearch
        result.state mustBe Job.Succeeded
        result.command.map(_.commandId) mustBe Some(command.id)
        completed.executionCount mustBe 1
        completed.activeRunId mustBe None
        completed.canBeExecuted mustBe false
      }
    }

    "persist investment research failures and bound their transient retries" in withEmbeddedMongoDatabase { db =>
      val services    = mock(classOf[Services[IO]])
      val screens     = mock(classOf[InvestmentScreenService[IO]])
      val evaluations = mock(classOf[InvestmentEvaluationService[IO]])
      val now         = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      when(services.investmentScreens).thenReturn(screens)
      when(services.investmentEvaluations).thenReturn(evaluations)
      when(evaluations.captureMonthlyEvaluations).thenReturn(IO.pure(0))
      when(screens.generateScheduledScreen).thenReturn(IO.raiseError(new java.io.IOException("research storage unavailable")))
      for {
        jobs      <- JobRepository.make[IO](db)
        _         <- enqueue(jobs)(Job(JobId("research-exhausted"), Action.RefreshInvestmentResearch, now, now, retries = 3))
        _         <- enqueue(jobs)(Job(JobId("research-retry"), Action.RefreshInvestmentResearch, now, now, retries = 2))
        executor  <- makeExecutor(jobs, services, ActionExecutorConfig(1))
        _         <- executor.run.take(2).compile.drain.timeout(3.seconds)
        exhausted <- jobs.find(JobId("research-exhausted")).map(_.get)
        retry     <- jobs.find(JobId("research-retry")).map(_.get)
      } yield {
        exhausted.state mustBe Job.Failed
        exhausted.retries mustBe 3
        exhausted.error mustBe Some("IOException")
        retry.state mustBe Job.Pending
        retry.retries mustBe 3
        retry.error mustBe Some("IOException")
        retry.availableAt.isAfter(now) mustBe true
      }
    }

    "finish a fan-out larger than the old queue capacity with one worker" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val profiles = mock(classOf[CompanyProfileService[IO]])
      val tickers  = (1 to 130).toList.map(n => Ticker(s"TEST$n"))
      when(services.companyProfile).thenReturn(profiles)
      tickers.foreach(t => when(profiles.get(t, true)).thenReturn(IO.pure(AAPLCompanyProfile)))
      for {
        jobs     <- JobRepository.make[IO](db)
        now      <- IO.realTimeInstant
        _        <- enqueue(jobs)(Job(JobId("fanout"), Action.UpdateCompanyProfiles(NonEmptyList.fromListUnsafe(tickers)), now, now))
        executor <- makeExecutor(jobs, services, ActionExecutorConfig(1))
        result   <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, JobId("fanout")))
        children <- jobs.summarize((0 until 130).toList.map(n => JobId(s"fanout/$n")))
      } yield {
        result.state mustBe Job.Succeeded
        children.count(Job.Succeeded) mustBe 130
      }
    }

    "wait for a sequence step to complete before executing the next" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val profiles = mock(classOf[CompanyProfileService[IO]])
      val security = mock(classOf[SecurityService[IO]])
      when(services.companyProfile).thenReturn(profiles)
      when(services.security).thenReturn(security)
      for {
        started <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        second  <- Ref.of[IO, Boolean](false)
        _ = when(profiles.get(AAPL, true)).thenReturn(started.complete(()).void >> release.get.as(AAPLCompanyProfile))
        _ = when(security.deactivate(AAPL)).thenReturn(second.set(true))
        jobs <- JobRepository.make[IO](db)
        now  <- IO.realTimeInstant
        _    <- enqueue(jobs)(
          Job(JobId("sequence"), Action.Sequence(NonEmptyList.of(Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)), action)), now, now)
        )
        executor <- makeExecutor(jobs, services, ActionExecutorConfig(3))
        result   <- executor.run.compile.drain.background.use { _ =>
          started.get.timeout(3.seconds) >> second.get.flatMap { early =>
            release.complete(()) >> awaitTerminal(jobs, JobId("sequence")).tupleLeft(early)
          }
        }
        ran <- second.get
      } yield {
        result._1 mustBe false
        result._2.state mustBe Job.Succeeded
        ran mustBe true
      }
    }

    "persist provider cooldown without consuming retries, and stop after three transient retries" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val profiles = mock(classOf[CompanyProfileService[IO]])
      val now      = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      when(services.companyProfile).thenReturn(profiles)
      when(profiles.get(AAPL, true))
        .thenReturn(IO.raiseError(AppError.ProviderDeferred(Provider.Finnhub, now.plusSeconds(300), "cooldown")))
      when(profiles.get(MSFT, true)).thenReturn(IO.raiseError(AppError.HttpClient(Provider.Finnhub.print, 503, "unavailable")))
      for {
        jobs      <- JobRepository.make[IO](db)
        _         <- enqueue(jobs)(Job(JobId("cooldown"), Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)), now, now, retries = 2))
        _         <- enqueue(jobs)(Job(JobId("exhausted"), Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, retries = 3))
        _         <- enqueue(jobs)(Job(JobId("retry"), Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, retries = 2))
        executor  <- makeExecutor(jobs, services, ActionExecutorConfig(1))
        _         <- executor.run.take(3).compile.drain.timeout(3.seconds)
        cooldown  <- jobs.find(JobId("cooldown")).map(_.get)
        exhausted <- jobs.find(JobId("exhausted")).map(_.get)
        retry     <- jobs.find(JobId("retry")).map(_.get)
      } yield {
        cooldown.state mustBe Job.Pending
        cooldown.availableAt mustBe now.plusSeconds(300)
        cooldown.retries mustBe 2
        exhausted.state mustBe Job.Failed
        exhausted.retries mustBe 3
        retry.state mustBe Job.Pending
        retry.retries mustBe 3
        retry.availableAt.isAfter(now) mustBe true
      }
    }

    "bound unavailable-data retries, retain partial results and release the scheduled occurrence" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val profiles = mock(classOf[CompanyProfileService[IO]])
      when(services.companyProfile).thenReturn(profiles)
      when(profiles.get(MSFT, true)).thenReturn(IO.raiseError(AppError.DataUnavailable(Provider.Finnhub, MSFT, "missing session")))
      for {
        jobs           <- JobRepository.make[IO](db)
        commands       <- CommandRepository.make[IO](db)
        dispatcher     <- ActionDispatcher.make(jobs)
        commandService <- CommandService.make(commands, dispatcher)
        _ = when(services.command).thenReturn(commandService)
        now     <- IO.realTimeInstant
        command <- commands.create(
          CreateCommand(Action.UpdateCompanyProfiles(NonEmptyList.of(AAPL, MSFT)), Schedule.Periodic(1.hour), Some(1))
        )
        ready      <- commands.initializeNextRun(command, now).map(_.get)
        occurrence <- commands.reserve(ready, now).map(_.get)
        parent = Job(
          occurrence.activeRunId.get,
          occurrence.action,
          now,
          now,
          Some(JobCommand(occurrence.id, occurrence.version, occurrence.activeRunId.get))
        )
        childIds = List(JobId(s"${parent.id.value}/0"), JobId(s"${parent.id.value}/1"))
        _ <- enqueue(jobs)(parent.copy(children = childIds, state = Job.Waiting, availableAt = now.plusSeconds(1)))
        _ <- enqueue(jobs)(
          Job(
            childIds.head,
            Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)),
            now,
            now,
            command = parent.command,
            state = Job.Succeeded
          )
        )
        _ <- enqueue(jobs)(
          Job(childIds.last, Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, command = parent.command, retries = 2)
        )
        executor  <- makeExecutor(jobs, services, ActionExecutorConfig(1))
        result    <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, parent.id))
        failed    <- jobs.find(childIds.last).map(_.get)
        _         <- CommandScheduler.make(commands, jobs).flatMap(_.tick)
        completed <- commands.find(command.id)
      } yield {
        failed.state mustBe Job.Failed
        failed.retries mustBe 3
        result.state mustBe Job.Partial
        completed.activeRunId mustBe None
        completed.executionCount mustBe 1
      }
    }

    "continue later sequence stages after a partially successful ticker refresh" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val profiles = mock(classOf[CompanyProfileService[IO]])
      val security = mock(classOf[SecurityService[IO]])
      when(services.companyProfile).thenReturn(profiles)
      when(services.security).thenReturn(security)
      when(profiles.get(AAPL, true)).thenReturn(IO.pure(AAPLCompanyProfile))
      when(profiles.get(MSFT, true)).thenReturn(IO.raiseError(AppError.CompanyProfileNotFound(MSFT)))
      for {
        ran <- Ref.of[IO, Boolean](false)
        _ = when(security.deactivate(AAPL)).thenReturn(ran.set(true))
        jobs <- JobRepository.make[IO](db)
        now  <- IO.realTimeInstant
        _    <- enqueue(jobs)(
          Job(
            JobId("partial"),
            Action.Sequence(NonEmptyList.of(Action.UpdateCompanyProfiles(NonEmptyList.of(AAPL, MSFT)), action)),
            now,
            now
          )
        )
        executor  <- makeExecutor(jobs, services, ActionExecutorConfig(4))
        result    <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, JobId("partial")))
        continued <- ran.get
      } yield {
        result.state mustBe Job.Partial
        continued mustBe true
      }
    }
    "recover a frozen plan after only some children were materialized" in withEmbeddedMongoDatabase { db =>
      val first  = Action.DeactivateSecurity(AAPL)
      val second = Action.DeactivateSecurity(MSFT)
      val plan   = JobPlan(List(first, second), sequential = false)
      for {
        jobs  <- JobRepository.make[IO](db)
        now   <- IO.realTimeInstant
        calls <- Ref.of[IO, List[Action]](Nil)
        parent = Job(
          JobId("interrupted"),
          Action.UpdateCompanyProfiles(NonEmptyList.of(AAPL, MSFT)),
          now,
          now,
          state = Job.Running,
          leaseToken = Some("expired"),
          leaseUntil = Some(now.minusSeconds(1)),
          plan = Some(plan)
        )
        _ <- enqueue(jobs)(parent)
        _ <- enqueue(jobs)(Job(JobId("interrupted/0"), first, now, now, state = Job.Succeeded, completedAt = Some(now)))
        handler = new ActionHandler[IO] {
          def execute(action: Action): IO[ActionResult] =
            calls.update(_ :+ action) >> IO
              .raiseUnless(action == second)(new IllegalStateException("Frozen work was recomputed"))
              .as(ActionResult.Completed)
        }
        executor   <- ActionExecutor.make(jobs, handler, _ => IO.pure(true), IO.pure(Set.empty[JobId]), ActionExecutorConfig(1))
        result     <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, parent.id))
        executed   <- calls.get
        firstChild <- jobs.find(JobId("interrupted/0")).map(_.get)
      } yield {
        result.state mustBe Job.Succeeded
        result.plan mustBe Some(plan)
        result.children mustBe List(JobId("interrupted/0"), JobId("interrupted/1"))
        firstChild.state mustBe Job.Succeeded
        executed mustBe List(second)
      }
    }

  }
}
