package stockschecker.controllers

import cats.effect.IO
import io.circe.parser.parse
import kirill5k.common.cats.Clock
import org.http4s.*
import org.http4s.implicits.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{CreateCommand, JobId, Schedule, UpdateCommand}
import stockschecker.services.CommandService
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.repositories.CommandRepository
import stockschecker.fixtures.*

import scala.concurrent.duration.*

class CommandControllerSpec extends HttpRoutesWordSpec {

  val testApiKey   = "test-api-key-12345"
  val apiConfig    = ApiConfig(testApiKey)
  val apiKeyHeader = Header.Raw(CIString("X-API-Key"), testApiKey)

  "A CommandController" when {
    "GET /commands" should {
      "return 200 and all commands on success" in {
        val svc = mocks
        when(svc.getAll).thenReturnIO(
          List(FetchLatestSecuritiesCommand.copy(version = 3L, nextRunAt = Some(ts), activeRunId = Some(JobId("active"))))
        )

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          req = Request[IO](uri = uri"/commands", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody =
          s"""[
             |  {
             |    "id" : "${FetchLatestSecuritiesCommand.id.value}",
             |    "isActive" : true,
             |    "action" : {
             |      "exchanges" : ["nasdaq"],
             |      "kind" : "fetch-securities"
             |    },
             |    "schedule" : {
             |      "kind" : "periodic",
             |      "period" : "20minutes"
             |    },
             |    "lastExecutedAt" : "${ts}",
             |    "executionCount" : 1,
             |    "maxExecutions" : 10
             |  }
             |]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAll
      }

      "return 401 when API key is missing" in {
        val svc = mocks

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          req = Request[IO](uri = uri"/commands", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }

      "return 401 when API key is invalid" in {
        val svc = mocks

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          invalidApiKeyHeader = Header.Raw(CIString("X-API-Key"), "invalid-api-key")
          req                 = Request[IO](uri = uri"/commands", method = Method.GET).withHeaders(invalidApiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }
    }

    "POST /commands" should {
      "accept an hourly investment research command" in {
        val svc   = mocks
        val input = CreateCommand(Action.RefreshInvestmentResearch, Schedule.Periodic(1.hour), None)
        when(svc.create(input)).thenReturnIO(FetchLatestSecuritiesCommand.copy(action = input.action, schedule = input.schedule))

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          body = """{"action":{"kind":"refresh-investment-research"},"schedule":{"kind":"periodic","period":"1hour"}}"""
          req  = Request[IO](uri = uri"/commands", method = Method.POST).withHeaders(apiKeyHeader).withBody(body)
          response <- controller.routes.orNotFound.run(req)
          _        <- IO(verify(svc).create(input))
        yield response

        res mustHaveStatus (Status.Created, Some(s"""{"id":"${FetchLatestSecuritiesCommand.id.value}"}"""))
      }

      "reject an unsupported action before writing a command" in {
        given Clock[IO] = Clock.mock(ts)
        val commands    = mock[CommandRepository[IO]]
        val dispatcher  = mock[ActionDispatcher[IO]]
        val res         = for
          service    <- CommandService.make(commands, dispatcher)
          controller <- CommandController.make(apiConfig, service)
          body = """{"action":{"kind":"unsupported"},"schedule":{"kind":"periodic","period":"20minutes"}}"""
          req  = Request[IO](uri = uri"/commands", method = Method.POST).withHeaders(apiKeyHeader).withBody(body)
          response <- controller.routes.orNotFound.run(req)
          _        <- IO(verifyNoInteractions(commands, dispatcher))
        yield response

        res mustHaveStatus (
          Status.UnprocessableContent,
          Some("""{"message":"type Action has no class/object/case named 'unsupported'."}""")
        )
      }

      "return 201 and command id on success" in {
        val svc = mocks
        when(svc.create(any[CreateCommand])).thenReturnIO(FetchLatestSecuritiesCommand)

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          body =
            """{
              |    "action" : {
              |      "exchanges" : ["nasdaq"],
              |      "kind" : "fetch-securities"
              |    },
              |    "schedule": {
              |        "kind": "periodic",
              |        "period" : "20minutes"
              |    },
              |    "lastExecutedAt" : "2025-10-29T10:06:06.961Z",
              |    "executionCount" : 1,
              |    "maxExecutions" : 10
              |}""".stripMargin
          req = Request[IO](uri = uri"/commands", method = Method.POST).withHeaders(apiKeyHeader).withBody(body)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Created, Some(s"""{"id" : "${FetchLatestSecuritiesCommand.id.value}"}"""))
      }

      "return 422 on invalid data" in {
        val svc = mocks
        when(svc.create(any[CreateCommand])).thenReturnIO(FetchLatestSecuritiesCommand)

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          body =
            """{
              |    "schedule": {
              |        "kind": "periodic",
              |        "period" : "20minutes"
              |    }
              |}""".stripMargin
          req = Request[IO](uri = uri"/commands", method = Method.POST).withHeaders(apiKeyHeader).withBody(body)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.UnprocessableContent, Some("""{"message" : "Missing required field: action"}"""))
      }

      "return 401 when API key is missing" in {
        val svc = mocks

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          body = """{"action":{"kind":"fetch-securities","exchanges":["nasdaq"]},"schedule":{"kind":"periodic","period":"20minutes"}}"""
          req  = Request[IO](uri = uri"/commands", method = Method.POST).withBody(body)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }
    }

    "PUT /commands/{id}" should {
      "return command configuration and execution data without scheduling internals" in {
        val svc     = mocks
        val command = FetchLatestSecuritiesCommand.copy(version = 3L, nextRunAt = Some(ts), activeRunId = Some(JobId("active")))
        when(svc.update(any[UpdateCommand])).thenReturnIO(command)
        val result = for
          controller <- CommandController.make(apiConfig, svc)
          body =
            """{"isActive":true,"action":{"kind":"fetch-securities","exchanges":["nasdaq"]},"schedule":{"kind":"periodic","period":"20minutes"},"maxExecutions":10}"""
          request = Request[IO](uri = uri"/commands" / command.id.value, method = Method.PUT).withHeaders(apiKeyHeader).withBody(body)
          response <- controller.routes.orNotFound.run(request)
          text     <- response.as[String]
          json     <- IO.fromEither(parse(text))
        yield (response.status, json)
        result
          .map { case (status, json) =>
            status mustBe Status.Ok
            json.asObject.get.keys.toSet mustBe Set(
              "id",
              "isActive",
              "action",
              "schedule",
              "lastExecutedAt",
              "executionCount",
              "maxExecutions"
            )
            json.hcursor.get[String]("id") mustBe Right(command.id.value)
            json.hcursor.get[Int]("executionCount") mustBe Right(command.executionCount)
          }
          .unsafeToFuture()(using cats.effect.unsafe.IORuntime.global)
      }
    }

    "POST /commands/{id}/execute" should {
      "return 204 on success" in {
        val svc       = mocks
        val commandId = FetchLatestSecuritiesCommand.id
        when(svc.executeManually(commandId)).thenReturnIO(())

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          req = Request[IO](uri = uri"/commands" / commandId.value / "execute", method = Method.POST).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).executeManually(commandId)
      }

      "return 404 when command not found" in {
        val svc       = mocks
        val commandId = FetchLatestSecuritiesCommand.id
        when(svc.executeManually(commandId)).thenRaiseError(new Exception("Command not found"))

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          req = Request[IO](uri = uri"/commands" / commandId.value / "execute", method = Method.POST).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.InternalServerError, Some("""{"message":"Command not found"}"""))
        verify(svc).executeManually(commandId)
      }

      "return 401 when API key is missing" in {
        val svc       = mocks
        val commandId = FetchLatestSecuritiesCommand.id

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          req = Request[IO](uri = uri"/commands" / commandId.value / "execute", method = Method.POST)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }

      "return 401 when API key is invalid" in {
        val svc       = mocks
        val commandId = FetchLatestSecuritiesCommand.id

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          invalidApiKeyHeader = Header.Raw(CIString("X-API-Key"), "invalid-api-key")
          req = Request[IO](uri = uri"/commands" / commandId.value / "execute", method = Method.POST).withHeaders(invalidApiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }

      "return 400 for invalid command id format" in {
        val svc = mocks

        val res = for
          controller <- CommandController.make(apiConfig, svc)
          req = Request[IO](uri = uri"/commands/invalid-id/execute", method = Method.POST).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.UnprocessableContent, Some("""{"message":"Invalid hexadecimal representation of an id: invalid-id"}"""))
        verifyNoInteractions(svc)
      }
    }
  }

  def mocks = mock[CommandService[IO]]
}
