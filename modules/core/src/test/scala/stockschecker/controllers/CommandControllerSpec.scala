package stockschecker.controllers

import cats.effect.IO
import org.http4s.*
import org.http4s.implicits.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import stockschecker.domain.CreateCommand
import stockschecker.services.CommandService
import stockschecker.fixtures.*

class CommandControllerSpec extends HttpRoutesWordSpec {

  "A CommandController" when {
    "GET /commands" should {
      "return 200 and all commands on success" in {
        val svc = mocks
        when(svc.getAll).thenReturnIO(List(FetchLatestSecuritiesCommand))

        val res = for
          controller <- CommandController.make(svc)
          req = Request[IO](uri = uri"/commands", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody =
          s"""[
             |  {
             |    "id" : "${FetchLatestSecuritiesCommand.id.value}",
             |    "isActive" : true,
             |    "action" : {
             |      "exchanges" : [
             |        "nasdaq"
             |      ],
             |      "kind" : "fetch-latest-securities"
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
    }

    "POST /commands" should {
      "return 201 and command id on success" in {
        val svc = mocks
        when(svc.create(any[CreateCommand])).thenReturnIO(FetchLatestSecuritiesCommand)

        val res = for
          controller <- CommandController.make(svc)
          body =
            """{
              |    "action" : {
              |      "exchanges" : [
              |        "nasdaq"
              |      ],
              |      "kind" : "fetch-latest-securities"
              |    },
              |    "schedule": {
              |        "kind": "periodic",
              |        "period" : "20minutes"
              |    },
          |        "lastExecutedAt" : "2025-10-29T10:06:06.961Z",
              |    "executionCount" : 1,
              |    "maxExecutions" : 10
              |}""".stripMargin
          req = Request[IO](uri = uri"/commands", method = Method.POST).withBody(body)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Created, Some(s"""{"id" : "${FetchLatestSecuritiesCommand.id.value}"}"""))
      }

      "return 422 on invalid data" in {
        val svc = mocks
        when(svc.create(any[CreateCommand])).thenReturnIO(FetchLatestSecuritiesCommand)

        val res = for
          controller <- CommandController.make(svc)
          body =
            """{
              |    "schedule": {
              |        "kind": "periodic",
              |        "period" : "20minutes"
              |    }
              |}""".stripMargin
          req = Request[IO](uri = uri"/commands", method = Method.POST).withBody(body)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.UnprocessableContent, Some("""{"message" : "Missing required field: action"}"""))
      }
    }
  }

  def mocks = mock[CommandService[IO]]
}
