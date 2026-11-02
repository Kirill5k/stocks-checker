package stockschecker.controllers

import cats.effect.IO
import io.circe.syntax.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.{Header, Request, Status}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.actions.Action
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.services.JobService

class JobControllerSpec extends HttpRoutesWordSpec:
  private val config = ApiConfig("test-key")
  private val apiKey = Header.Raw(CIString("X-API-Key"), config.key)
  private val job = JobView.from(Job(JobId("command:2:1793599200000/245"), Action.RefreshInvestmentResearch, ts, ts))

  "Job endpoints" should {
    List(uri"/jobs", uri"/jobs/status?id=run%2F0").foreach { uri =>
      s"require authentication for $uri" in {
        val service = mock[JobService[IO]]
        val response = for
          controller <- JobController.make(service, config)
          result <- controller.routes.orNotFound.run(Request[IO](uri = uri))
        yield result
        response mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(service)
      }
    }

    "default to the twenty latest jobs" in {
      val service = mock[JobService[IO]]
      when(service.list(20, JobFilter())).thenReturnIO(List(job))
      val response = for
        controller <- JobController.make(service, config)
        result <- controller.routes.orNotFound.run(Request[IO](uri = uri"/jobs").withHeaders(apiKey))
      yield result
      response mustHaveStatus (Status.Ok, Some(List(job).asJson.noSpaces))
      verify(service).list(20, JobFilter())
    }

    "decode the bounded limit and exact command, state and action filters" in {
      val service = mock[JobService[IO]]
      val filter = JobFilter(Some(FetchLatestSecuritiesCommand.id), Some(Job.Partial), Some("refresh-investment-research"))
      when(service.list(5, filter)).thenReturnIO(Nil)
      val uri = uri"/jobs"
        .withQueryParam("limit", 5)
        .withQueryParam("commandId", FetchLatestSecuritiesCommand.id.value)
        .withQueryParam("state", "partial")
        .withQueryParam("actionKind", "refresh-investment-research")
      val response = for
        controller <- JobController.make(service, config)
        result <- controller.routes.orNotFound.run(Request[IO](uri = uri).withHeaders(apiKey))
      yield result
      response mustHaveStatus (Status.Ok, Some("[]"))
      verify(service).list(5, filter)
    }

    List(1, 100).foreach { limit =>
      s"accept the boundary limit of $limit" in {
        val service = mock[JobService[IO]]
        when(service.list(limit, JobFilter())).thenReturnIO(Nil)
        val response = for
          controller <- JobController.make(service, config)
          result <- controller.routes.orNotFound.run(
            Request[IO](uri = uri"/jobs".withQueryParam("limit", limit)).withHeaders(apiKey)
          )
        yield result
        response mustHaveStatus (Status.Ok, Some("[]"))
        verify(service).list(limit, JobFilter())
      }
    }

    "look up slash-containing job ids without treating them as path segments" in {
      val service = mock[JobService[IO]]
      when(service.get(job.id)).thenReturnIO(job)
      val response = for
        controller <- JobController.make(service, config)
        result <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/jobs/status".withQueryParam("id", job.id.value)).withHeaders(apiKey)
        )
      yield result
      response mustHaveStatus (Status.Ok, Some(job.asJson.noSpaces))
      verify(service).get(job.id)
    }

    "return 404 for a missing or pruned job" in {
      val service = mock[JobService[IO]]
      val id = JobId("missing/0")
      val error = AppError.EntityDoesNotExist("Job", id.value)
      when(service.get(id)).thenRaiseError(error)
      val response = for
        controller <- JobController.make(service, config)
        result <- controller.routes.orNotFound.run(
          Request[IO](uri = uri"/jobs/status".withQueryParam("id", id.value)).withHeaders(apiKey)
        )
      yield result
      response mustHaveStatus (Status.NotFound, Some(ErrorResponse(error.getMessage).asJson.noSpaces))
    }

    List(
      ("unknown state", uri"/jobs?state=unknown", "Unknown job state: unknown"),
      ("invalid command id", uri"/jobs?commandId=invalid", "Invalid hexadecimal representation of an id: invalid"),
      ("zero limit", uri"/jobs?limit=0", "limit must be between 1 and 100"),
      ("excessive limit", uri"/jobs?limit=101", "limit must be between 1 and 100"),
      ("empty job id", uri"/jobs/status".withQueryParam("id", ""), "Job id must not be empty"),
      ("oversized job id", uri"/jobs/status".withQueryParam("id", "a" * 1025), "Job id must not exceed 1024 characters"),
      ("empty action kind", uri"/jobs".withQueryParam("actionKind", ""), "actionKind must not be empty"),
      ("blank action kind", uri"/jobs".withQueryParam("actionKind", " "), "actionKind must not be empty"),
      ("oversized action kind", uri"/jobs".withQueryParam("actionKind", "a" * 101), "actionKind must not exceed 100 characters")
    ).foreach { case (description, uri, message) =>
      s"reject $description before reading jobs" in {
        val service = mock[JobService[IO]]
        val response = for
          controller <- JobController.make(service, config)
          result <- controller.routes.orNotFound.run(Request[IO](uri = uri).withHeaders(apiKey))
        yield result
        response mustHaveStatus (Status.UnprocessableContent, Some(ErrorResponse(message).asJson.noSpaces))
        verifyNoInteractions(service)
      }
    }
  }
