package stockschecker.repositories

import cats.effect.IO
import kirill5k.common.cats.Clock
import cats.syntax.foldable.*
import stockschecker.actions.{Action, ActionPriority}
import stockschecker.domain.*
import stockschecker.fixtures.*

class JobRepositoryListingSpec extends RepositorySpec:
  given Clock[IO]        = Clock.mock(ts)
  override def port: Int = 12175

  private def enqueue(jobs: JobRepository[IO])(job: Job): IO[Unit] =
    jobs.enqueue(job, ActionPriority.forAction(job.action))

  "JobRepository listing" should {
    "return the newest jobs with deterministic descending ID ties and a bounded result" in withEmbeddedMongoDatabase { db =>
      val action = Action.RefreshInvestmentResearch
      val rows   = List(
        Job(JobId("old"), action, ts, ts.minusSeconds(1)),
        Job(JobId("a"), action, ts, ts),
        Job(JobId("z"), action, ts, ts),
        Job(JobId("new"), action, ts, ts.plusSeconds(1))
      )
      for
        jobs    <- JobRepository.make[IO](db)
        _       <- rows.traverse_(enqueue(jobs))
        limited <- jobs.list(2, JobFilter())
        all     <- jobs.list(20, JobFilter())
      yield
        limited.map(_.id) mustBe List("new", "z").map(JobId(_))
        all.map(_.id) mustBe List("new", "z", "a", "old").map(JobId(_))
        all.find(_.id == JobId("old")) mustBe Some(rows.head)
    }

    "combine exact command, state and action-kind filters" in withEmbeddedMongoDatabase { db =>
      val commandId    = FetchLatestSecuritiesCommand.id
      val otherCommand = CommandId("000000000000000000000001")
      val matching     = Job(
        JobId("matching"),
        Action.RefreshInvestmentResearch,
        ts,
        ts,
        command = Some(JobCommand(commandId, 1, JobId("run"))),
        state = Job.Failed,
        error = Some("Provider data unavailable"),
        completedAt = Some(ts)
      )
      val rows = List(
        matching,
        matching.copy(id = JobId("other-command"), command = Some(JobCommand(otherCommand, 1, JobId("other-run")))),
        matching.copy(id = JobId("other-state"), state = Job.Succeeded),
        matching.copy(id = JobId("other-action"), action = Action.RefreshInvestmentRanking),
        matching.copy(id = JobId("manual"), command = None)
      )
      for
        jobs      <- JobRepository.make[IO](db)
        _         <- rows.traverse_(enqueue(jobs))
        combined  <- jobs.list(20, JobFilter(Some(commandId), Some(Job.Failed), Some("refresh-investment-research")))
        byCommand <- jobs.list(20, JobFilter(commandId = Some(commandId)))
        byState   <- jobs.list(20, JobFilter(state = Some(Job.Succeeded)))
        byAction  <- jobs.list(20, JobFilter(actionKind = Some("refresh-investment-research")))
        prefix    <- jobs.list(20, JobFilter(actionKind = Some("refresh-investment")))
      yield
        combined mustBe List(matching)
        byCommand.map(_.id).toSet mustBe Set("matching", "other-state", "other-action").map(JobId(_))
        byState.map(_.id) mustBe List(JobId("other-state"))
        byAction.map(_.id).toSet mustBe Set("matching", "other-command", "other-state", "manual").map(JobId(_))
        prefix mustBe Nil
    }

    "find slash-containing job IDs exactly and preserve optional status metadata" in withEmbeddedMongoDatabase { db =>
      val id  = JobId("command:2:1793599200000/245")
      val row = Job(
        id,
        Action.RefreshInvestmentResearch,
        ts.plusSeconds(30),
        ts,
        dependsOn = List(JobId("command:2:1793599200000/244")),
        children = List(JobId(s"${id.value}/0")),
        state = Job.Paused,
        retries = 2,
        waitCount = 4,
        error = Some("Provider configuration required"),
        pausedProvider = Some(Provider.TwelveData)
      )
      for
        jobs   <- JobRepository.make[IO](db)
        _      <- enqueue(jobs)(row)
        exact  <- jobs.find(id)
        prefix <- jobs.find(JobId("command:2:1793599200000"))
        listed <- jobs.list(20, JobFilter())
      yield
        exact mustBe Some(row)
        prefix mustBe None
        listed mustBe List(row)
        listed.head.command mustBe None
        listed.head.completedAt mustBe None
    }
  }
