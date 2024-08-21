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
        when(svc.getAll).thenReturnIO(List(FetchLatestStocksCommand))

        val res = for
          controller <- CommandController.make(svc)
          req = Request[IO](uri = uri"/commands", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val responseBody =
          s"""[
             |  {
             |    "id" : "${FetchLatestStocksCommand.id.value}",
             |    "isActive" : true,
             |    "action" : {
             |      "kind" : "fetch-latest-stocks"
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
        when(svc.create(any[CreateCommand])).thenReturnIO(FetchLatestStocksCommand)

        val res = for
          controller <- CommandController.make(svc)
          body =
            """{
              |    "action": {
              |        "kind": "fetch-latest-stocks"
              |    },
              |    "schedule": {
              |        "kind": "periodic",
              |        "period" : "20minutes"
              |    }
              |}""".stripMargin
          req = Request[IO](uri = uri"/commands", method = Method.POST).withBody(body)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus(Status.Created, Some(s"""{"id" : "${FetchLatestStocksCommand.id.value}"}"""))
      }

      "return 422 on invalid data" in {
        val svc = mocks
        when(svc.create(any[CreateCommand])).thenReturnIO(FetchLatestStocksCommand)

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

        res mustHaveStatus(Status.UnprocessableEntity, Some("""{"message" : "Missing required field: action"}"""))
      }
    }
  }

  def mocks = mock[CommandService[IO]]
}
