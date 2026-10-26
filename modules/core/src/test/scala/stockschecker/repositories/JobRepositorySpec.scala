package stockschecker.repositories

import cats.data.NonEmptyList
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import kirill5k.common.cats.Clock
import org.mockito.Mockito.{mock, when}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.actions.{Action, ActionDispatcher, ActionExecutor, CommandScheduler}
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.{CreateCommand, Job, JobCommand, Schedule, Ticker, UpdateCommand}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.services.{CommandService, CompanyProfileService, SecurityService, Services}

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class JobRepositorySpec extends RepositorySpec {
  override def port: Int = 12156
  given Clock[IO] = Clock.make[IO]
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val action = Action.RecordCompanyProfileUpdate(List(AAPL))
  private def awaitTerminal(repo: JobRepository[IO], id: String): IO[Job] =
    repo.find(id).flatMap {
      case Some(job) if job.isTerminal => IO.pure(job)
      case _ => IO.sleep(30.millis) >> awaitTerminal(repo, id)
    }.timeout(10.seconds)

  "Durable jobs" should {
    "enqueue idempotently, claim once, recover expired leases and fence stale owners" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs <- JobRepository.make[IO](db)
        _ <- jobs.enqueue(Job("same", action, now, now))
        _ <- jobs.enqueue(Job("same", Action.RescheduleAll, now, now))
        claims <- (jobs.claim(now, 2.minutes), jobs.claim(now, 2.minutes)).parTupled
        first = List(claims._1, claims._2).flatten.head
        all <- jobs.list()
        recovered <- jobs.claim(now.plusSeconds(121), 2.minutes).map(_.get)
        _ <- jobs.finish(first, Job.Succeeded)
        fenced <- jobs.find("same").map(_.get)
        _ <- jobs.finish(recovered, Job.Succeeded)
        completed <- jobs.find("same").map(_.get)
      } yield {
        List(claims._1, claims._2).flatten.size mustBe 1
        all.size mustBe 1
        first.action mustBe action
        recovered.leaseToken must not be first.leaseToken
        fenced.state mustBe Job.Running
        completed.state mustBe Job.Succeeded
      }
    }

    "recover the command outbox after a crash without counting the occurrence twice" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs <- JobRepository.make[IO](db)
        commands <- CommandRepository.make[IO](db)
        cmd <- commands.create(CreateCommand(action, Schedule.Periodic(1.hour), Some(1)))
        _ <- commands.initializeNextRun(cmd, now)
        due <- commands.find(cmd.id)
        reserved <- commands.reserve(due, now)
        before <- jobs.list()
        _ <- CommandScheduler.tick(commands, jobs)
        _ <- CommandScheduler.tick(commands, jobs)
        after <- commands.find(cmd.id)
        all <- jobs.list()
        claimed <- jobs.claim(Instant.now(), 2.minutes).map(_.get)
        _ <- jobs.finish(claimed, Job.Succeeded)
        _ <- CommandScheduler.tick(commands, jobs)
        exhausted <- commands.find(cmd.id)
      } yield {
        reserved must not be empty
        before mustBe empty
        all.size mustBe 1
        after.executionCount mustBe 1
        after.outbox mustBe None
        exhausted.activeRunId mustBe None
        exhausted.nextRunAt mustBe None
        exhausted.canBeExecuted mustBe false
      }
    }

    "never cancel a newer occurrence from an older scheduler snapshot" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      val commandId = FetchLatestSecuritiesCommand.id
      for {
        jobs <- JobRepository.make[IO](db)
        _ <- jobs.enqueue(Job("old", action, now, now, Some(JobCommand(commandId, 0, "old"))))
        _ <- jobs.enqueue(Job("new", action, now, now, Some(JobCommand(commandId, 2, "new"))))
        _ <- jobs.cancelObsolete(commandId, 1)
        old <- jobs.find("old").map(_.get)
        newer <- jobs.find("new").map(_.get)
      } yield {
        old.state mustBe Job.Cancelled
        newer.state mustBe Job.Pending
      }
    }

    "cancel queued old versions on edits and deactivate, and schedule again on activation" in withEmbeddedMongoDatabase { db =>
      for {
        jobs <- JobRepository.make[IO](db)
        commands <- CommandRepository.make[IO](db)
        dispatcher <- ActionDispatcher.durable(jobs)
        service <- CommandService.make(commands, dispatcher)
        cmd <- service.create(CreateCommand(action, Schedule.Periodic(1.hour), None))
        _ <- CommandScheduler.tick(commands, jobs)
        before <- commands.find(cmd.id)
        _ <- service.update(UpdateCommand(cmd.id, true, Action.RecordCompanyProfileUpdate(List(MSFT)), Schedule.Periodic(1.second), None))
        old <- jobs.find(before.activeRunId.get).map(_.get)
        _ <- service.activate(cmd.id, false)
        _ <- CommandScheduler.tick(commands, jobs)
        inactive <- commands.find(cmd.id)
        _ <- service.activate(cmd.id, true)
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
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      val services = mock(classOf[Services[IO]])
      val security = mock(classOf[SecurityService[IO]])
      when(services.security).thenReturn(security)
      when(security.recordCompanyProfileUpdate(List(AAPL))).thenReturn(IO.unit)
      for {
        jobs <- JobRepository.make[IO](db)
        _ <- (1 to 16).toList.traverse_(n => jobs.enqueue(Job(s"timer-$n", Action.Schedule(FetchLatestSecuritiesCommand.id, 1.day), now.plusSeconds(86400), now)))
        _ <- jobs.enqueue(Job("ready", action, now, now))
        dispatcher <- ActionDispatcher.durable(jobs)
        executor <- ActionExecutor.make(dispatcher, services, ActionExecutorConfig(16))
        _ <- executor.run.take(1).compile.drain.timeout(3.seconds)
        ready <- jobs.find("ready").map(_.get)
        timers <- jobs.list()
      } yield {
        ready.state mustBe Job.Succeeded
        timers.count(_.state == Job.Pending) mustBe 16
      }
    }

    "finish a fan-out larger than the old queue capacity with one worker" in withEmbeddedMongoDatabase { db =>
      val services = mock(classOf[Services[IO]])
      val profiles = mock(classOf[CompanyProfileService[IO]])
      val tickers = (1 to 130).toList.map(n => Ticker(s"TEST$n"))
      when(services.companyProfile).thenReturn(profiles)
      tickers.foreach(t => when(profiles.get(t, true)).thenReturn(IO.pure(AAPLCompanyProfile)))
      for {
        jobs <- JobRepository.make[IO](db)
        now <- IO.realTimeInstant
        _ <- jobs.enqueue(Job("fanout", Action.UpdateCompanyProfiles(NonEmptyList.fromListUnsafe(tickers)), now, now))
        dispatcher <- ActionDispatcher.durable(jobs)
        executor <- ActionExecutor.make(dispatcher, services, ActionExecutorConfig(1))
        result <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, "fanout"))
        children <- jobs.list(200)
      } yield {
        result.state mustBe Job.Succeeded
        children.count(_.state == Job.Succeeded) mustBe 131
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
        second <- Ref.of[IO, Boolean](false)
        _ = when(profiles.get(AAPL, true)).thenReturn(started.complete(()).void >> release.get.as(AAPLCompanyProfile))
        _ = when(security.recordCompanyProfileUpdate(List(AAPL))).thenReturn(second.set(true))
        jobs <- JobRepository.make[IO](db)
        now <- IO.realTimeInstant
        _ <- jobs.enqueue(Job("sequence", Action.Sequence(NonEmptyList.of(Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)), action)), now, now))
        dispatcher <- ActionDispatcher.durable(jobs)
        executor <- ActionExecutor.make(dispatcher, services, ActionExecutorConfig(3))
        result <- executor.run.compile.drain.background.use { _ =>
          started.get.timeout(3.seconds) >> second.get.flatMap { early =>
            release.complete(()) >> awaitTerminal(jobs, "sequence").tupleLeft(early)
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
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      when(services.companyProfile).thenReturn(profiles)
      when(profiles.get(AAPL, true)).thenReturn(IO.raiseError(AppError.ProviderDeferred("Finnhub", now.plusSeconds(300), "cooldown")))
      when(profiles.get(MSFT, true)).thenReturn(IO.raiseError(AppError.HttpClient("Finnhub", 503, "unavailable")))
      for {
        jobs <- JobRepository.make[IO](db)
        _ <- jobs.enqueue(Job("cooldown", Action.UpdateCompanyProfiles(NonEmptyList.one(AAPL)), now, now, retries = 2))
        _ <- jobs.enqueue(Job("exhausted", Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, retries = 3))
        _ <- jobs.enqueue(Job("retry", Action.UpdateCompanyProfiles(NonEmptyList.one(MSFT)), now, now, retries = 2))
        dispatcher <- ActionDispatcher.durable(jobs)
        executor <- ActionExecutor.make(dispatcher, services, ActionExecutorConfig(1))
        _ <- executor.run.take(3).compile.drain.timeout(3.seconds)
        cooldown <- jobs.find("cooldown").map(_.get)
        exhausted <- jobs.find("exhausted").map(_.get)
        retry <- jobs.find("retry").map(_.get)
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
        _ = when(security.recordCompanyProfileUpdate(List(AAPL))).thenReturn(ran.set(true))
        jobs <- JobRepository.make[IO](db)
        now <- IO.realTimeInstant
        _ <- jobs.enqueue(Job("partial", Action.Sequence(NonEmptyList.of(Action.UpdateCompanyProfiles(NonEmptyList.of(AAPL, MSFT)), action)), now, now))
        dispatcher <- ActionDispatcher.durable(jobs)
        executor <- ActionExecutor.make(dispatcher, services, ActionExecutorConfig(4))
        result <- executor.run.compile.drain.background.use(_ => awaitTerminal(jobs, "partial"))
        continued <- ran.get
      } yield {
        result.state mustBe Job.Partial
        continued mustBe true
      }
    }
  }
}
