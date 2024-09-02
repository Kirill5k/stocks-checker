package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import stockschecker.domain.Ticker
import stockschecker.services.StockService
import stockschecker.fixtures.*

class StockControllerSpec extends HttpRoutesWordSpec {

  "A StockController" when {
    "GET /stocks/:ticker" should {
      "return 200 and company stock on success" in {
        val svc = mocks
        when(svc.get(any[Ticker], anyOpt[Int])).thenReturnIO(List(AAPLStock))

        val res = for
          controller <- StockController.make(svc)
          req = Request[IO](uri = uri"/stocks/AAPL", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val resBody = s"""[{
                        |  "ticker" : "AAPL",
                        |  "price" : 234.4,
                        |  "priceDelta" : null,
                        |  "stockType" : "stock",
                        |  "lastUpdatedAt" : "${ts}"
                        |}]""".stripMargin
        res mustHaveStatus(Status.Ok, Some(resBody))
        verify(svc).get(AAPL, None)
      }
    }
  }

  def mocks: StockService[IO] = mock[StockService[IO]]
}
