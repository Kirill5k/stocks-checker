package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import mongo4cats.operations.Filter
import org.mockito.Mockito.{mock, when}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher, ActionExecutor, CommandScheduler}
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.{
  CreateCommand,
  EvaluationCapturePlan,
  InvestmentScreenId,
  Job,
  JobCommand,
  JobId,
  JobPlan,
  Schedule,
  Ticker,
  UpdateCommand
}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.services.{CommandService, CompanyProfileService, InvestmentScreenService, SecurityService, Services}

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class JobRepositorySpec extends RepositorySpec {
  override def port: Int = 12156
  given Clock[IO]        = Clock.make[IO]
  given Logger[IO]       = Slf4jLogger.getLogger[IO]

  private val action            = Action.DeactivateSecurity(AAPL)
  private val evaluationThrough = LocalDate.parse("2026-01-01")
  private val evaluationAction  = Action.RefreshEvaluationPrices(AAPL, LocalDate.parse("2013-01-01"), evaluationThrough)
  private def awaitTerminal(repo: JobRepository[IO], id: JobId): IO[Job] =
    repo
      .find(id)
      .flatMap {
        case Some(job) if job.isTerminal => IO.pure(job)
        case _                           => IO.sleep(30.millis) >> awaitTerminal(repo, id)
      }
      .timeout(10.seconds)

  "Durable jobs" should {

    "bound canonical evaluation restarts across failures and repository restarts" in withEmbeddedMongoDatabase { db =>
      val later = Instant.now().plusSeconds(10.days.toSeconds)
      for {
        jobs          <- JobRepository.make[IO](db)
        _             <- jobs.enqueue(Job(JobId("bounded"), evaluationAction, ts, ts, state = Job.Failed, completedAt = Some(ts)))
        first         <- jobs.retryTerminal(JobId("bounded"), later)
        firstAttempt  <- jobs.claim(later, 2.minutes).map(_.get)
        _             <- jobs.finish(firstAttempt, Job.Failed)
        restarted     <- JobRepository.make[IO](db)
        second        <- (restarted.retryTerminal(JobId("bounded"), later), restarted.retryTerminal(JobId("bounded"), later)).parTupled
        secondAttempt <- restarted.claim(later, 2.minutes).map(_.get)
        _             <- restarted.finish(secondAttempt, Job.Failed)
        exhausted     <- restarted.retryTerminal(JobId("bounded"), later)
        finalJob      <- restarted.find(JobId("bounded")).map(_.get)
      } yield {
        first mustBe true
        firstAttempt.restartCount mustBe Some(1)
        List(second._1, second._2).count(identity) mustBe 1
        secondAttempt.restartCount mustBe Some(Job.MaxEvaluationRestarts)
        exhausted mustBe false
        finalJob.state mustBe Job.Failed
        finalJob.restartCount mustBe Some(Job.MaxEvaluationRestarts)
      }
    }

    "retain evaluation attempt counts while a capture needs its cutoff, then prune after it settles" in withEmbeddedMongoDatabase { db =>
      val expiredAt = ts.minusSeconds(31.days.toSeconds)
      for {
        jobs        <- JobRepository.make[IO](db)
        evaluations <- InvestmentEvaluationRepository.make[IO](db)
        _           <- jobs.enqueue(
          Job(
            JobId("needed"),
            evaluationAction,
            expiredAt,
            expiredAt,
            state = Job.Failed,
            completedAt = Some(expiredAt),
            restartCount = Some(Job.MaxEvaluationRestarts)
          )
        )
        _ <- evaluations.getOrCreateCapturePlan(EvaluationCapturePlan(InvestmentScreenId("screen"), "2026-02", evaluationThrough, ts))
        _ <- jobs.pruneCompleted(ts)
        protectedJob <- jobs.find(JobId("needed"))
        plans        <- db.getCollection(InvestmentEvaluationRepository.PlanCollectionName)
        _            <- plans.deleteMany(Filter.empty)
        _            <- jobs.pruneCompleted(ts)
        expired      <- jobs.find(JobId("needed"))
      } yield {
        protectedJob.flatMap(_.restartCount) mustBe Some(Job.MaxEvaluationRestarts)
        expired mustBe None
      }
    }

    "aggregate child outcomes while preserving missing children as unfinished" in withEmbeddedMongoDatabase { db =>
      for {
        jobs <- JobRepository.make[IO](db)
        _    <- List(
          Job(JobId("parent/0"), action, ts, ts, state = Job.Succeeded),
          Job(JobId("parent/1"), action, ts, ts, state = Job.Failed),
          Job(JobId("parent/2"), action, ts, ts, state = Job.Paused)
        ).traverse_(jobs.enqueue)
        summary  <- jobs.summarize(List("parent/0", "parent/1", "parent/2", "parent/missing").map(JobId(_)))
        terminal <- jobs.terminalIds(List("parent/0", "parent/1", "parent/2", "parent/missing").map(JobId(_)))
      } yield {
        summary.total mustBe 3
        summary.terminal mustBe 2
        summary.usable mustBe 1
        summary.count(Job.Paused) mustBe 1
        terminal mustBe Set(JobId("parent/0"), JobId("parent/1"))
      }
    }

    "claim normal updates before evaluation backfills and break due-time ties deterministically" in withEmbeddedMongoDatabase { db =>
      val evaluation = Action.RefreshEvaluationPrices(AAPL, LocalDate.parse("2020-01-01"), LocalDate.parse("2026-01-01"))
      for {
        jobs <- JobRepository.make[IO](db)
        _    <- List(
          Job(JobId("z"), action, ts, ts),
          Job(JobId("a"), action, ts, ts),
          Job(JobId("first"), action, ts.minusSeconds(1), ts),
          Job(JobId("evaluation"), evaluation, ts.minusSeconds(60), ts),
          Job(JobId("future"), action, ts.plusSeconds(60), ts)
        ).traverse_(jobs.enqueue)
        claimed <- List.fill(4)(()).traverse(_ => jobs.claim(ts, 2.minutes))
        none    <- jobs.claim(ts, 2.minutes)
      } yield {
        claimed.flatten.map(_.id) mustBe List("first", "a", "z", "evaluation").map(JobId(_))
        none mustBe None
      }
    }

    "back off waiting parents without consuming execution retries" in withEmbeddedMongoDatabase { db =>
      for {
        jobs      <- JobRepository.make[IO](db)
        _         <- jobs.enqueue(Job(JobId("parent"), action, ts, ts))
        first     <- jobs.claim(ts, 2.minutes).map(_.get)
        _         <- jobs.waitForChildren(first, List(JobId("parent/0")), ts)
        waiting   <- jobs.find(JobId("parent")).map(_.get)
        second    <- jobs.claim(ts.plusSeconds(1), 2.minutes).map(_.get)
        _         <- jobs.waitForChildren(second, second.children, ts.plusSeconds(1))
        backedOff <- jobs.find(JobId("parent")).map(_.get)
      } yield {
        waiting.availableAt mustBe ts.plusSeconds(1)
        backedOff.availableAt mustBe ts.plusSeconds(3)
        backedOff.retries mustBe 0
        backedOff.copy(waitCount = Some(30)).nextPollDelaySeconds mustBe 300L
      }
    }

    "retain completed children of unfinished parents and prune completed trees together" in withEmbeddedMongoDatabase { db =>
      val expiredAt = ts.minusSeconds(31.days.toSeconds)
      for {
        jobs <- JobRepository.make[IO](db)
        _    <- List(
          Job(JobId("waiting"), action, ts, expiredAt, children = List(JobId("waiting/0")), state = Job.Waiting),
          Job(JobId("waiting/0"), action, expiredAt, expiredAt, state = Job.Succeeded, completedAt = Some(expiredAt)),
          Job(
            JobId("finished"),
            action,
            expiredAt,
            expiredAt,
            children = List(JobId("finished/0")),
            state = Job.Partial,
            completedAt = Some(expiredAt)
          ),
          Job(JobId("finished/0"), action, expiredAt, expiredAt, state = Job.Failed, completedAt = Some(expiredAt)),
          Job(JobId("recent"), action, ts, ts, state = Job.Succeeded, completedAt = Some(ts))
        ).traverse_(jobs.enqueue)
        _            <- jobs.pruneCompleted(ts)
        kept         <- jobs.find(JobId("waiting/0"))
        removedRoot  <- jobs.find(JobId("finished"))
        removedChild <- jobs.find(JobId("finished/0"))
        recent       <- jobs.find(JobId("recent"))
      } yield {
        kept must not be empty
        removedRoot mustBe None
        removedChild mustBe None
        recent must not be empty
      }
    }

    "retry a terminal evaluation once with a daily cooldown and clear its retention clock" in withEmbeddedMongoDatabase { db =>
      for {
        jobs <- JobRepository.make[IO](db)
        _    <- jobs.enqueue(Job(JobId("evaluation"), evaluationAction, ts, ts, state = Job.Failed, retries = 3, completedAt = Some(ts)))
        _    <- jobs.enqueue(Job(JobId("planned"), action, ts, ts, state = Job.Failed, plan = Some(JobPlan(List(action), false))))
        _    <- jobs.enqueue(Job(JobId("parent/0"), action, ts, ts, state = Job.Failed))
        rejected <- List("planned", "parent/0").map(JobId(_)).traverse(jobs.retryTerminal(_, ts))
        retries  <- (jobs.retryTerminal(JobId("evaluation"), ts), jobs.retryTerminal(JobId("evaluation"), ts)).parTupled
        queued   <- jobs.find(JobId("evaluation")).map(_.get)
        early    <- jobs.claim(ts.plusSeconds(1), 2.minutes)
        due      <- jobs.claim(ts.plusSeconds(1.day.toSeconds), 2.minutes)
      } yield {
        rejected mustBe List(false, false)
        List(retries._1, retries._2).count(identity) mustBe 1
        queued.state mustBe Job.Pending
        queued.completedAt mustBe None
        queued.retries mustBe 0
        queued.restartCount mustBe Some(1)
        queued.availableAt mustBe ts.plusSeconds(1.day.toSeconds)
        early mustBe None
        due.map(_.id) mustBe Some(JobId("evaluation"))
      }
    }
    "enqueue idempotently, claim once, recover expired leases and fence stale owners" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs   <- JobRepository.make[IO](db)
        _      <- jobs.enqueue(Job(JobId("same"), action, now, now))
        _      <- jobs.enqueue(Job(JobId("same"), Action.DeactivateSecurity(MSFT), now, now))
        claims <- (jobs.claim(now, 2.minutes), jobs.claim(now, 2.minutes)).parTupled
        first = List(claims._1, claims._2).flatten.head
        recovered <- jobs.claim(now.plusSeconds(121), 2.minutes).map(_.get)
        _         <- jobs.finish(first, Job.Succeeded)
        fenced    <- jobs.find(JobId("same")).map(_.get)
        _         <- jobs.finish(recovered, Job.Succeeded)
        completed <- jobs.find(JobId("same")).map(_.get)
      } yield {
        List(claims._1, claims._2).flatten.size mustBe 1
        first.action mustBe action
        recovered.leaseToken must not be first.leaseToken
        fenced.state mustBe Job.Running
        completed.state mustBe Job.Succeeded
      }
    }

    "recover the command outbox after a crash without counting the occurrence twice" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs      <- JobRepository.make[IO](db)
        commands  <- CommandRepository.make[IO](db)
        cmd       <- commands.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
        _         <- commands.initializeNextRun(cmd, now)
        due       <- commands.find(cmd.id)
        reserved  <- commands.reserve(due, now)
        before    <- jobs.find(reserved.get.job.id)
        _         <- CommandScheduler.tick(commands, jobs)
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

    "never cancel a newer occurrence from an older scheduler snapshot" in withEmbeddedMongoDatabase { db =>
      val now       = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      val commandId = FetchLatestSecuritiesCommand.id
      for {
        jobs  <- JobRepository.make[IO](db)
        _     <- jobs.enqueue(Job(JobId("old"), action, now, now, Some(JobCommand(commandId, 0, JobId("old")))))
        _     <- jobs.enqueue(Job(JobId("new"), action, now, now, Some(JobCommand(commandId, 2, JobId("new")))))
        _     <- jobs.cancelObsolete(commandId, 1)
        old   <- jobs.find(JobId("old")).map(_.get)
        newer <- jobs.find(JobId("new")).map(_.get)
      } yield {
        old.state mustBe Job.Cancelled
        newer.state mustBe Job.Pending
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

    "run ready work with sixteen future schedules and no free timer workers" in withEmbeddedMongoDatabase { db =>
      val now      = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      val services = mock(classOf[Services[IO]])
      val security = mock(classOf[SecurityService[IO]])
      when(services.security).thenReturn(security)
      when(security.deactivate(AAPL)).thenReturn(IO.unit)
      for {
        jobs     <- JobRepository.make[IO](db)
        _        <- (1 to 16).toList.traverse_(n => jobs.enqueue(Job(JobId(s"timer-$n"), action, now.plusSeconds(86400), now)))
        _        <- jobs.enqueue(Job(JobId("ready"), action, now, now))
        executor <- ActionExecutor.make(jobs, services, ActionExecutorConfig(16))
        _        <- executor.run.take(1).compile.drain.timeout(3.seconds)
        ready    <- jobs.find(JobId("ready")).map(_.get)
        timers   <- jobs.summarize((1 to 16).toList.map(n => JobId(s"timer-$n")))
      } yield {
        ready.state mustBe Job.Succeeded
        timers.count(Job.Pending) mustBe 16
      }
    }

    "execute scheduled investment research and release its completed occurrence" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val screens  = mock(classOf[InvestmentScreenService[IO]])
      when(services.investmentScreens).thenReturn(screens)
      for {
        runs <- Ref.of[IO, Int](0)
        _ = when(screens.refreshResearch).thenReturn(runs.update(_ + 1))
        jobs           <- JobRepository.make[IO](db)
        commands       <- CommandRepository.make[IO](db)
        dispatcher     <- ActionDispatcher.durable(jobs)
        commandService <- CommandService.make(commands, dispatcher, jobs)
        _ = when(services.command).thenReturn(commandService)
        command <- commandService.create(CreateCommand(Action.RefreshInvestmentResearch, Schedule.Periodic(1.hour), Some(1)))
        _       <- CommandScheduler.tick(commands, jobs)
        queued  <- commands.find(command.id)
        runId = queued.activeRunId.get
        executor  <- ActionExecutor.make(jobs, services, ActionExecutorConfig(1))
        _         <- executor.run.take(1).compile.drain.timeout(3.seconds)
        result    <- jobs.find(runId).map(_.get)
        count     <- runs.get
        _         <- CommandScheduler.tick(commands, jobs)
        completed <- commands.find(command.id)
      } yield {
        count mustBe 1
        result.action mustBe Action.RefreshInvestmentResearch
        result.state mustBe Job.Succeeded
        result.command.map(_.commandId) mustBe Some(command.id)
        completed.executionCount mustBe 1
        completed.activeRunId mustBe None
        completed.outbox mustBe None
        completed.canBeExecuted mustBe false
      }
    }

    "persist investment research failures and bound their transient retries" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val screens  = mock(classOf[InvestmentScreenService[IO]])
      val now      = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      when(services.investmentScreens).thenReturn(screens)
      when(screens.refreshResearch).thenReturn(IO.raiseError(new java.io.IOException("research storage unavailable")))
      for {
        jobs      <- JobRepository.make[IO](db)
        _         <- jobs.enqueue(Job(JobId("research-exhausted"), Action.RefreshInvestmentResearch, now, now, retries = 3))
        _         <- jobs.enqueue(Job(JobId("research-retry"), Action.RefreshInvestmentResearch, now, now, retries = 2))
        executor  <- ActionExecutor.make(jobs, services, ActionExecutorConfig(1))
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
        _        <- jobs.enqueue(Job(JobId("fanout"), Action.UpdateCompanyProfiles(NonEmptyList.fromListUnsafe(tickers)), now, now))
        executor <- ActionExecutor.make(jobs, services, ActionExecutorConfig(1))
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
        _    <- jobs.enqueue(
          Job(JobId("sequence"), Action.Sequence(NonEmptyList.of(Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)), action)), now, now)
        )
        executor <- ActionExecutor.make(jobs, services, ActionExecutorConfig(3))
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
      when(profiles.get(AAPL, true)).thenReturn(IO.raiseError(AppError.ProviderDeferred("Finnhub", now.plusSeconds(300), "cooldown")))
      when(profiles.get(MSFT, true)).thenReturn(IO.raiseError(AppError.HttpClient("Finnhub", 503, "unavailable")))
      for {
        jobs      <- JobRepository.make[IO](db)
        _         <- jobs.enqueue(Job(JobId("cooldown"), Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)), now, now, retries = 2))
        _         <- jobs.enqueue(Job(JobId("exhausted"), Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, retries = 3))
        _         <- jobs.enqueue(Job(JobId("retry"), Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, retries = 2))
        executor  <- ActionExecutor.make(jobs, services, ActionExecutorConfig(1))
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
      when(profiles.get(MSFT, true)).thenReturn(IO.raiseError(AppError.DataUnavailable("Finnhub", MSFT, "missing session")))
      for {
        jobs           <- JobRepository.make[IO](db)
        commands       <- CommandRepository.make[IO](db)
        dispatcher     <- ActionDispatcher.durable(jobs)
        commandService <- CommandService.make(commands, dispatcher, jobs)
        _ = when(services.command).thenReturn(commandService)
        now     <- IO.realTimeInstant
        command <- commands.create(
          CreateCommand(Action.UpdateCompanyProfiles(NonEmptyList.of(AAPL, MSFT)), Schedule.Periodic(1.hour), Some(1))
        )
        ready      <- commands.initializeNextRun(command, now).map(_.get)
        occurrence <- commands.reserve(ready, now).map(_.get)
        parent   = occurrence.job
        childIds = List(JobId(s"${parent.id.value}/0"), JobId(s"${parent.id.value}/1"))
        _ <- jobs.enqueue(parent.copy(children = childIds, state = Job.Waiting, availableAt = now.plusSeconds(1)))
        _ <- jobs.enqueue(
          Job(
            childIds.head,
            Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)),
            now,
            now,
            command = parent.command,
            state = Job.Succeeded
          )
        )
        _ <- jobs.enqueue(
          Job(childIds.last, Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, command = parent.command, retries = 2)
        )
        executor  <- ActionExecutor.make(jobs, services, ActionExecutorConfig(1))
        result    <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, parent.id))
        failed    <- jobs.find(childIds.last).map(_.get)
        _         <- CommandScheduler.tick(commands, jobs)
        completed <- commands.find(command.id)
      } yield {
        failed.state mustBe Job.Failed
        failed.retries mustBe 3
        result.state mustBe Job.Partial
        completed.activeRunId mustBe None
        completed.outbox mustBe None
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
        _    <- jobs.enqueue(
          Job(
            JobId("partial"),
            Action.Sequence(NonEmptyList.of(Action.UpdateCompanyProfiles(NonEmptyList.of(AAPL, MSFT)), action)),
            now,
            now
          )
        )
        executor  <- ActionExecutor.make(jobs, services, ActionExecutorConfig(4))
        result    <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, JobId("partial")))
        continued <- ran.get
      } yield {
        result.state mustBe Job.Partial
        continued mustBe true
      }
    }
  }
}
