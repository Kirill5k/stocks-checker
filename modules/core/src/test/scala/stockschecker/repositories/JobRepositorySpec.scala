package stockschecker.repositories

import cats.effect.IO
import kirill5k.common.cats.Clock
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import mongo4cats.errors.MongoJsonParsingException
import mongo4cats.operations.{Filter, Update}
import stockschecker.actions.{Action, ActionPriority}
import stockschecker.domain.*
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

class JobRepositorySpec extends RepositorySpec {
  given Clock[IO]                                                  = Clock.mock(ts)
  override def port: Int                                           = 12156
  private val action                                               = Action.SetSecurityActive(AAPL, false, java.time.Instant.EPOCH)
  private val evaluationThrough                                    = LocalDate.parse("2026-01-01")
  private val evaluationAction                                     = Action.RefreshEvaluationPrices(AAPL, evaluationThrough)
  private def enqueue(jobs: JobRepository[IO])(job: Job): IO[Unit] = jobs.enqueue(job, ActionPriority.forAction(job.action))

  "Durable jobs" should {
    "reject a null wait count while decoding a valid failed research job" in withEmbeddedMongoDatabase { db =>
      val valid = Job(
        JobId("valid-research"),
        Action.RefreshInvestmentResearch,
        ts,
        ts,
        state = Job.Failed,
        retries = 3,
        waitCount = 3,
        completedAt = Some(ts)
      )
      val invalid = valid.copy(id = JobId("null-wait-count"))
      for {
        jobs     <- JobRepository.make[IO](db)
        _        <- enqueue(jobs)(valid)
        _        <- enqueue(jobs)(invalid)
        raw      <- db.getCollection(JobRepository.CollectionName)
        _        <- raw.updateOne(Filter.idEq(invalid.id.value), Update.set("job.waitCount", Option.empty[Int]))
        decoded  <- jobs.find(valid.id)
        rejected <- jobs.find(invalid.id).attempt
      } yield {
        decoded mustBe Some(valid)
        rejected.left.toOption.get mustBe a[MongoJsonParsingException]
        rejected.left.toOption.get.getMessage must include("job.waitCount")
      }
    }

    "require the current counter schema in persisted jobs" in withEmbeddedMongoDatabase { db =>
      val previousSchema = Job(JobId("previous-schema"), Action.RefreshInvestmentResearch, ts, ts)
      for
        jobs   <- JobRepository.make[IO](db)
        _      <- enqueue(jobs)(previousSchema)
        raw    <- db.getCollection(JobRepository.CollectionName)
        _      <- raw.updateOne(Filter.idEq(previousSchema.id.value), Update.unset("job.deferralCount").unset("job.reclaimCount"))
        result <- jobs.find(previousSchema.id).attempt
      yield result.left.toOption.get mustBe a[MongoJsonParsingException]
    }

    "preserve sub-millisecond activation request timestamps through persistence and claiming" in withEmbeddedMongoDatabase { db =>
      val event = Action.SetSecurityActive(AAPL, true, ts.plusNanos(123456))
      for
        jobs    <- JobRepository.make[IO](db)
        _       <- enqueue(jobs)(Job(JobId("activation"), event, ts, ts))
        saved   <- jobs.find(JobId("activation"))
        claimed <- jobs.claim(ts, 2.minutes)
      yield
        saved.map(_.action) mustBe Some(event)
        claimed.map(_.action) mustBe Some(event)
    }

    "prune expired evaluation history jobs without consulting domain state" in withEmbeddedMongoDatabase { db =>
      val expiredAt = ts.minusSeconds(31.days.toSeconds)
      for {
        jobs <- JobRepository.make[IO](db)
        _    <- enqueue(jobs)(
          Job(
            JobId("needed"),
            evaluationAction,
            expiredAt,
            expiredAt,
            state = Job.Failed,
            completedAt = Some(expiredAt)
          )
        )
        _       <- jobs.pruneCompleted(ts)
        expired <- jobs.find(JobId("needed"))
      } yield expired mustBe None
    }

    "aggregate child outcomes while preserving missing children as unfinished" in withEmbeddedMongoDatabase { db =>
      for {
        jobs <- JobRepository.make[IO](db)
        _    <- List(
          Job(JobId("parent/0"), action, ts, ts, state = Job.Succeeded),
          Job(JobId("parent/1"), action, ts, ts, state = Job.Failed),
          Job(JobId("parent/2"), action, ts, ts, state = Job.Paused)
        ).traverse_(enqueue(jobs))
        summary <- jobs.summarize(List("parent/0", "parent/1", "parent/2", "parent/missing").map(JobId(_)))
      } yield {
        summary.total mustBe 3
        summary.terminal mustBe 2
        summary.usable mustBe 1
        summary.count(Job.Paused) mustBe 1
      }
    }

    "claim internal events before fan-out work, then evaluation backfills, breaking due-time ties deterministically" in withEmbeddedMongoDatabase {
      db =>
        val normal     = Action.UpdatePriceAnalytics(cats.data.NonEmptyList.one(AAPL))
        val evaluation = Action.RefreshEvaluationPrices(AAPL, LocalDate.parse("2026-01-01"))
        for {
          jobs <- JobRepository.make[IO](db)
          _    <- List(
            Job(JobId("z"), normal, ts, ts),
            Job(JobId("a"), normal, ts, ts),
            Job(JobId("first"), normal, ts.minusSeconds(1), ts),
            Job(JobId("internal"), action, ts, ts),
            Job(JobId("evaluation"), evaluation, ts.minusSeconds(60), ts),
            Job(JobId("future"), action, ts.plusSeconds(60), ts)
          ).traverse_(enqueue(jobs))
          claimed <- List.fill(5)(()).traverse(_ => jobs.claim(ts, 2.minutes))
          none    <- jobs.claim(ts, 2.minutes)
        } yield {
          claimed.flatten.map(_.id) mustBe List("internal", "first", "a", "z", "evaluation").map(JobId(_))
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
        waiting.waitCount mustBe 1
        backedOff.availableAt mustBe ts.plusSeconds(3)
        backedOff.waitCount mustBe 2
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
          Job(JobId("finished0"), action, ts, ts),
          Job(JobId("finished-extra/0"), action, ts, ts),
          Job(JobId("recent"), action, ts, ts, state = Job.Succeeded, completedAt = Some(ts))
        ).traverse_(enqueue(jobs))
        _            <- jobs.pruneCompleted(ts)
        kept         <- jobs.find(JobId("waiting/0"))
        removedRoot  <- jobs.find(JobId("finished"))
        removedChild <- jobs.find(JobId("finished/0"))
        recent       <- jobs.find(JobId("recent"))
        neighbor     <- jobs.find(JobId("finished0"))
        otherChild   <- jobs.find(JobId("finished-extra/0"))
      } yield {
        kept must not be empty
        removedRoot mustBe None
        removedChild mustBe None
        recent must not be empty
        neighbor must not be empty
        otherChild must not be empty
      }
    }

    "enqueue idempotently, claim once, recover expired leases and fence stale owners" in withEmbeddedMongoDatabase { db =>
      val now = Instant.now().truncatedTo(ChronoUnit.MILLIS)
      for {
        jobs   <- JobRepository.make[IO](db)
        _      <- enqueue(jobs)(Job(JobId("same"), action, now, now))
        _      <- enqueue(jobs)(Job(JobId("same"), Action.SetSecurityActive(MSFT, false, java.time.Instant.EPOCH), now, now))
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

    "fail a repeatedly abandoned lease after the bounded reclaim allowance" in withEmbeddedMongoDatabase { db =>
      for
        jobs      <- JobRepository.make[IO](db)
        _         <- enqueue(jobs)(Job(JobId("poison"), action, ts, ts))
        first     <- jobs.claim(ts, 1.second).map(_.get)
        recovered <- (1 to JobRepository.MaxLeaseReclaims).toList.traverse(n => jobs.claim(ts.plusSeconds(n.toLong), 1.second))
        exhausted <- jobs.claim(ts.plusSeconds(JobRepository.MaxLeaseReclaims.toLong + 1), 1.second)
        failed    <- jobs.find(first.id).map(_.get)
      yield
        first.reclaimCount mustBe 0
        recovered.flatten.map(_.reclaimCount) mustBe (1 to JobRepository.MaxLeaseReclaims).toList
        exhausted mustBe None
        failed.state mustBe Job.Failed
        failed.completedAt mustBe Some(ts.plusSeconds(JobRepository.MaxLeaseReclaims.toLong + 1))
        failed.error mustBe Some("Lease recovery limit exceeded")
    }

    "resume paused credentials after their cooldown and retain pending parents" in withEmbeddedMongoDatabase { db =>
      val retryAt = ts.plusMillis(Provider.configurationRetryDelay.toMillis)
      for
        jobs    <- JobRepository.make[IO](db)
        _       <- enqueue(jobs)(Job(JobId("paused"), action, ts, ts))
        running <- jobs.claim(ts, 2.minutes).map(_.get)
        _       <- jobs.pause(running, Provider.Finnhub, "invalid credentials")
        paused  <- jobs.find(running.id).map(_.get)
        pending <- jobs.pendingIds(List(running.id))
        early   <- jobs.claim(retryAt.minusMillis(1), 2.minutes)
        resumed <- jobs.claim(retryAt, 2.minutes).map(_.get)
      yield
        paused.state mustBe Job.Paused
        paused.availableAt mustBe retryAt
        pending mustBe Set(running.id)
        early mustBe None
        resumed.state mustBe Job.Running
        resumed.pausedProvider mustBe None
        resumed.retries mustBe 0
    }

    "use the application clock for terminal timestamps and reject renewal after expiry" in withEmbeddedMongoDatabase { db =>
      for
        jobs    <- JobRepository.make[IO](db)
        _       <- enqueue(jobs)(Job(JobId("clock"), action, ts, ts))
        running <- jobs.claim(ts, 1.second).map(_.get)
        renewed <- jobs.renew(running, ts.plusSeconds(1), 1.second)
        _       <- jobs.finish(running, Job.Succeeded)
        done    <- jobs.find(running.id).map(_.get)
      yield
        renewed mustBe false
        done.completedAt mustBe Some(ts)
    }

    "count provider deferrals independently of child waits and transient retries" in withEmbeddedMongoDatabase { db =>
      for
        jobs     <- JobRepository.make[IO](db)
        _        <- enqueue(jobs)(Job(JobId("counters"), action, ts, ts, waitCount = 20, retries = 2))
        first    <- jobs.claim(ts, 2.minutes).map(_.get)
        _        <- jobs.defer(first, ts.plusSeconds(1), Some("rate limited"), countRetry = false, countDeferral = true)
        deferred <- jobs.find(first.id).map(_.get)
        second   <- jobs.claim(ts.plusSeconds(1), 2.minutes).map(_.get)
        _        <- jobs.defer(second, ts.plusSeconds(2), Some("transport error"), countRetry = true)
        retry    <- jobs.find(first.id).map(_.get)
      yield
        deferred.waitCount mustBe 20
        deferred.deferralCount mustBe 1
        deferred.retries mustBe 2
        retry.waitCount mustBe 20
        retry.deferralCount mustBe 1
        retry.retries mustBe 3
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

  }
}
