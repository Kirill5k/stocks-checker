package stockschecker.repositories

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import mongo4cats.operations.Filter
import stockschecker.actions.{Action, ActionPriority}
import stockschecker.domain.*
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class JobRepositorySpec extends RepositorySpec {
  override def port: Int        = 12156
  private val action            = Action.DeactivateSecurity(AAPL)
  private val evaluationThrough = LocalDate.parse("2026-01-01")
  private val evaluationAction  = Action.RefreshEvaluationPrices(AAPL, LocalDate.parse("2013-01-01"), evaluationThrough)
  private def enqueue(jobs: JobRepository[IO])(job: Job): IO[Unit] = jobs.enqueue(job, ActionPriority.forAction(job.action))

  "Durable jobs" should {
    "retain evaluation attempt counts while a capture needs its cutoff, then prune after it settles" in withEmbeddedMongoDatabase { db =>
      val expiredAt = ts.minusSeconds(31.days.toSeconds)
      for {
        jobs         <- JobRepository.make[IO](db)
        capturePlans <- EvaluationCapturePlanRepository.make[IO](db)
        _            <- enqueue(jobs)(
          Job(
            JobId("needed"),
            evaluationAction,
            expiredAt,
            expiredAt,
            state = Job.Failed,
            completedAt = Some(expiredAt),
            restartCount = Some(2)
          )
        )
        _            <- capturePlans.getOrCreate(EvaluationCapturePlan(InvestmentScreenId("screen"), "2026-02", evaluationThrough, ts))
        _            <- jobs.pruneCompleted(ts)
        protectedJob <- jobs.find(JobId("needed"))
        plans        <- db.getCollection(EvaluationCapturePlanRepository.CollectionName)
        _            <- plans.deleteMany(Filter.empty)
        _            <- jobs.pruneCompleted(ts)
        expired      <- jobs.find(JobId("needed"))
      } yield {
        protectedJob.flatMap(_.restartCount) mustBe Some(2)
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
        ).traverse_(enqueue(jobs))
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
        ).traverse_(enqueue(jobs))
        claimed <- List.fill(4)(()).traverse(_ => jobs.claim(ts, 2.minutes))
        none    <- jobs.claim(ts, 2.minutes)
      } yield {
        claimed.flatten.map(_.id) mustBe List("first", "a", "z", "evaluation").map(JobId(_))
        none mustBe None
      }
    }

    "persist parent polling deadlines and wait counts without consuming execution retries" in withEmbeddedMongoDatabase { db =>
      for {
        jobs      <- JobRepository.make[IO](db)
        _         <- enqueue(jobs)(Job(JobId("parent"), action, ts, ts))
        first     <- jobs.claim(ts, 2.minutes).map(_.get)
        _         <- jobs.waitForChildren(first, List(JobId("parent/0")), ts.plusSeconds(1))
        waiting   <- jobs.find(JobId("parent")).map(_.get)
        second    <- jobs.claim(ts.plusSeconds(1), 2.minutes).map(_.get)
        _         <- jobs.waitForChildren(second, second.children, ts.plusSeconds(3))
        backedOff <- jobs.find(JobId("parent")).map(_.get)
      } yield {
        waiting.availableAt mustBe ts.plusSeconds(1)
        waiting.waitCount mustBe Some(1)
        backedOff.availableAt mustBe ts.plusSeconds(3)
        backedOff.waitCount mustBe Some(2)
        backedOff.retries mustBe 0
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
        ).traverse_(enqueue(jobs))
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

    "enqueue idempotently, claim once, recover expired leases and fence stale owners" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs   <- JobRepository.make[IO](db)
        _      <- enqueue(jobs)(Job(JobId("same"), action, now, now))
        _      <- enqueue(jobs)(Job(JobId("same"), Action.DeactivateSecurity(MSFT), now, now))
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

    "never cancel a newer occurrence from an older scheduler snapshot" in withEmbeddedMongoDatabase { db =>
      val now       = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      val commandId = FetchLatestSecuritiesCommand.id
      for {
        jobs  <- JobRepository.make[IO](db)
        _     <- enqueue(jobs)(Job(JobId("old"), action, now, now, Some(JobCommand(commandId, 0, JobId("old")))))
        _     <- enqueue(jobs)(Job(JobId("new"), action, now, now, Some(JobCommand(commandId, 2, JobId("new")))))
        _     <- jobs.cancelObsolete(commandId, 1)
        old   <- jobs.find(JobId("old")).map(_.get)
        newer <- jobs.find(JobId("new")).map(_.get)
      } yield {
        old.state mustBe Job.Cancelled
        newer.state mustBe Job.Pending
      }
    }

    "restart a terminal root only once from the expected persisted state" in withEmbeddedMongoDatabase { db =>
      val expected    = Job(JobId("restart"), action, ts, ts, state = Job.Failed, retries = 3, completedAt = Some(ts))
      val availableAt = ts.plusSeconds(60)
      val guarded     = List(
        expected.copy(id = JobId("planned"), plan = Some(JobPlan(List(action), false))),
        expected.copy(id = JobId("parent"), children = List(JobId("parent/0"))),
        expected.copy(id = JobId("parent/0")),
        expected.copy(id = JobId("pending"), state = Job.Pending)
      )
      for {
        jobs            <- JobRepository.make[IO](db)
        _               <- (expected :: guarded).traverse_(enqueue(jobs))
        rejected        <- guarded.traverse(jobs.restart(_, availableAt))
        wrongCompletion <- jobs.restart(expected.copy(completedAt = Some(ts.minusSeconds(1))), availableAt)
        wrongCount      <- jobs.restart(expected.copy(restartCount = Some(1)), availableAt)
        attempts        <- (jobs.restart(expected, availableAt), jobs.restart(expected, availableAt)).parTupled
        restarted       <- jobs.find(expected.id).map(_.get)
      } yield {
        rejected mustBe List(false, false, false, false)
        wrongCompletion mustBe false
        wrongCount mustBe false
        List(attempts._1, attempts._2).count(identity) mustBe 1
        restarted.state mustBe Job.Pending
        restarted.availableAt mustBe availableAt
        restarted.restartCount mustBe Some(1)
        restarted.retries mustBe 0
        restarted.completedAt mustBe None
      }
    }

  }
}
