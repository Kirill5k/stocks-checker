package stockschecker.domain

import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.actions.Action
import stockschecker.fixtures.*

class JobViewSpec extends AnyWordSpec with Matchers:
  "JobView" should {
    "expose execution status without lease or plan internals" in {
      val command = JobCommand(FetchLatestSecuritiesCommand.id, 3, JobId("run"))
      val job     = Job(
        JobId("run/0"),
        Action.CreateMonthlyScreen,
        ts.plusSeconds(30),
        ts,
        command = Some(command),
        dependsOn = List(JobId("run/before")),
        children = List(JobId("run/0/0")),
        state = Job.Paused,
        retries = 2,
        leaseToken = Some("private-token"),
        leaseUntil = Some(ts.plusSeconds(60)),
        error = Some("Provider unavailable"),
        pausedProvider = Some(Provider.TwelveData),
        plan = Some(JobPlan(List(Action.CaptureMonthlyEvaluations), true)),
        waitCount = 4
      )
      val view = JobView.from(job)
      view.commandId mustBe Some(command.commandId)
      view.runId mustBe Some(command.runId)
      view.dependsOn mustBe job.dependsOn
      view.children mustBe job.children
      view.pausedProvider mustBe job.pausedProvider
      view.retries mustBe 2
      view.waitCount mustBe 4
      view.asJson.asObject.get.keys.toSet mustBe Set(
        "id",
        "action",
        "state",
        "createdAt",
        "availableAt",
        "completedAt",
        "retries",
        "waitCount",
        "commandId",
        "runId",
        "dependsOn",
        "children",
        "pausedProvider",
        "error"
      )
      view.asJson.noSpaces must not include "private-token"
    }

    "redact credential fields while retaining useful provider diagnostics" in {
      val error =
        """TwelveData HTTP 400 for ACA https://example.test?apikey=secret-one&token=secret-two {"api_key":"secret-three","password":"secret-four"} Authorization: Bearer secret-five Basic secret-six"""
      val view     = JobView.from(Job(JobId("failed"), Action.RefreshInvestmentResearch, ts, ts, error = Some(error)))
      val redacted = view.error.get
      List("secret-one", "secret-two", "secret-three", "secret-four", "secret-five", "secret-six")
        .foreach(secret => redacted must not include secret)
      redacted must include("TwelveData HTTP 400 for ACA")
      redacted must include("[redacted]")
    }
  }
