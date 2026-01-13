package stockschecker.controllers

import cats.effect.IO
import org.http4s.*
import org.http4s.implicits.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.CreateCommand
import stockschecker.services.CommandService
import stockschecker.fixtures.*

class CommandControllerSpec extends HttpRoutesWordSpec {

  val testApiKey   = "test-api-key-12345"
  val apiConfig    = ApiConfig(testApiKey)
  val apiKeyHeader = Header.Raw(CIString("X-API-Key"), testApiKey)

  "A CommandController" when {
    "GET /commands" should {
      "return 200 and all commands on success" in {
        val svc = mocks
        when(svc.getAll).thenReturnIO(List(FetchLatestSecuritiesCommand))

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
