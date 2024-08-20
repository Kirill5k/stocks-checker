package stockschecker.controllers

import cats.effect.IO
import org.http4s.*
import org.http4s.implicits.*
import kirill5k.common.http4s.test.HttpRoutesWordSpec
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
        res mustHaveStatus(Status.Ok, Some(responseBody))
        verify(svc).getAll
      }
    }
  }

  def mocks = mock[CommandService[IO]]
}
