package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import stockschecker.domain.{Exchange, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.services.SecurityService
import stockschecker.fixtures.*

class SecurityControllerSpec extends HttpRoutesWordSpec {

  "A SecurityController" when {
    "GET /securities/:ticker" should {
      "return 200 and security on success" in {
        val svc = mocks
        when(svc.findByTicker(any[Ticker])).thenReturnIO(AAPLSecurity)

        val res = for
          controller <- SecurityController.make(svc)
          req = Request[IO](uri = uri"/securities/aapl", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val resBody = s"""{
                        |  "ticker" : "AAPL",
                        |  "exchange" : "nasdaq",
                        |  "name" : "Apple Inc.",
                        |  "kind" : "stock",
                        |  "isActive" : true
                        |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(resBody))
        verify(svc).findByTicker(AAPL)
      }

      "return 404 when security is not found" in {
        val svc = mocks
        val ticker = Ticker("UNKNOWN")
        when(svc.findByTicker(ticker)).thenRaiseError(AppError.SecurityNotFound(ticker))

        val res = for
          controller <- SecurityController.make(svc)
          req = Request[IO](uri = uri"/securities/unknown", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val resBody = s"""{"message" : "Could not find security for UNKNOWN"}""".stripMargin
        res mustHaveStatus (Status.NotFound, Some(resBody))
        verify(svc).findByTicker(ticker)
      }
    }

    "GET /securities/exchange/:exchange" should {
      "return 200 and list of securities on success" in {
        val svc = mocks
        when(svc.findByExchange(any[Exchange])).thenReturnIO(List(AAPLSecurity, MSFTSecurity))

        val res = for
          controller <- SecurityController.make(svc)
          req = Request[IO](uri = uri"/securities/exchange/nasdaq", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val resBody =
          s"""[
             |  {
             |    "ticker" : "AAPL",
             |    "exchange" : "nasdaq",
             |    "name" : "Apple Inc.",
             |    "kind" : "stock",
             |    "isActive" : true
             |  },
             |  {
             |    "ticker" : "MSFT",
             |    "exchange" : "nasdaq",
             |    "name" : "Microsoft Corporation",
             |    "kind" : "stock",
             |    "isActive" : true
             |  }
             |]""".stripMargin

        res mustHaveStatus(Status.Ok, Some(resBody))
        verify(svc).findByExchange(Exchange.NASDAQ)
      }
    }
  }

  def mocks: SecurityService[IO] = mock[SecurityService[IO]]
}
