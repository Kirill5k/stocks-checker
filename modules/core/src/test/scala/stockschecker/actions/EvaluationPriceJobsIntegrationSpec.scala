package stockschecker.actions

import cats.effect.IO
import cats.syntax.parallel.*
import stockschecker.domain.{Job, JobId, JobPlan}
import stockschecker.fixtures.*
import stockschecker.repositories.{JobRepository, RepositorySpec}

import java.time.{Instant, LocalDate}
import scala.concurrent.duration.*

class EvaluationPriceJobsIntegrationSpec extends RepositorySpec {
  override def port: Int = 12212

  private val through = LocalDate.parse("2026-01-01")
  private val id      = EvaluationPriceJobs.id(AAPL, through)
  private val action  = Action.RefreshEvaluationPrices(AAPL, LocalDate.parse("2013-01-01"), through)

  "Durable evaluation repairs" should {
    "bound canonical restarts across concurrent callers and repository restarts" in withEmbeddedMongoDatabase { db =>
      val later = Instant.now().plusSeconds(10.days.toSeconds)
      for {
        repository    <- JobRepository.make[IO](db)
        jobs          <- EvaluationPriceJobs.make(repository)
        _             <- repository.enqueue(Job(id, action, ts, ts, state = Job.Failed, completedAt = Some(ts)), ActionPriority.Evaluation)
        first         <- jobs.ensure(AAPL, through, later)
        firstAttempt  <- repository.claim(later, 2.minutes).map(_.get)
        _             <- repository.finish(firstAttempt, Job.Failed)
        restarted     <- JobRepository.make[IO](db)
        repairs       <- EvaluationPriceJobs.make(restarted)
        second        <- (repairs.ensure(AAPL, through, later), repairs.ensure(AAPL, through, later)).parTupled
        secondAttempt <- restarted.claim(later, 2.minutes).map(_.get)
        duplicate     <- restarted.claim(later, 2.minutes)
        _             <- restarted.finish(secondAttempt, Job.Failed)
        exhausted     <- repairs.ensure(AAPL, through, later)
        finalJob      <- restarted.find(id).map(_.get)
      } yield {
        first mustBe Some(id)
        firstAttempt.restartCount mustBe Some(1)
        second mustBe (Some(id), Some(id))
        secondAttempt.restartCount mustBe Some(EvaluationPriceJobs.MaxRestarts)
        duplicate mustBe None
        exhausted mustBe None
        finalJob.state mustBe Job.Failed
        finalJob.restartCount mustBe Some(EvaluationPriceJobs.MaxRestarts)
      }
    }

    "apply the daily cooldown once, clear the retention clock and reject stale restart attempts" in withEmbeddedMongoDatabase { db =>
      val previous = Job(id, action, ts, ts, state = Job.Failed, retries = 3, completedAt = Some(ts))
      val planned  = Job(JobId("planned"), action, ts, ts, state = Job.Failed, plan = Some(JobPlan(List(action), false)))
      val child    = Job(JobId("parent/0"), action, ts, ts, state = Job.Failed)
      for {
        repository  <- JobRepository.make[IO](db)
        jobs        <- EvaluationPriceJobs.make(repository)
        _           <- repository.enqueue(previous, ActionPriority.Evaluation)
        _           <- repository.enqueue(planned, ActionPriority.Evaluation)
        _           <- repository.enqueue(child, ActionPriority.Evaluation)
        rejectPlan  <- repository.restart(planned, ts)
        rejectChild <- repository.restart(child, ts)
        retries     <- (jobs.ensure(AAPL, through, ts), jobs.ensure(AAPL, through, ts)).parTupled
        stale       <- repository.restart(previous, ts)
        queued      <- repository.find(id).map(_.get)
        early       <- repository.claim(ts.plusSeconds(1), 2.minutes)
        due         <- repository.claim(ts.plusSeconds(1.day.toSeconds), 2.minutes)
      } yield {
        rejectPlan mustBe false
        rejectChild mustBe false
        retries mustBe (Some(id), Some(id))
        stale mustBe false
        queued.state mustBe Job.Pending
        queued.completedAt mustBe None
        queued.retries mustBe 0
        queued.restartCount mustBe Some(1)
        queued.availableAt mustBe ts.plusSeconds(1.day.toSeconds)
        early mustBe None
        due.map(_.id) mustBe Some(id)
      }
    }

    "reuse one persisted ticker and cutoff job across producers and process restarts" in withEmbeddedMongoDatabase { db =>
      for {
        repository <- JobRepository.make[IO](db)
        first      <- EvaluationPriceJobs.make(repository)
        accepted   <- first.ensure(AAPL, through, ts)
        reopened   <- JobRepository.make[IO](db)
        second     <- EvaluationPriceJobs.make(reopened)
        reused     <- second.ensure(AAPL, through, ts.plusSeconds(60))
        job        <- reopened.claim(ts, 2.minutes)
        duplicate  <- reopened.claim(ts.plusSeconds(60), 2.minutes)
      } yield {
        accepted mustBe Some(id)
        reused mustBe accepted
        job.map(_.id) mustBe accepted
        job.map(_.createdAt) mustBe Some(ts)
        duplicate mustBe None
      }
    }
  }
}
