package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{Exchange, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.services.SecurityService
import stockschecker.fixtures.*

class SecurityControllerSpec extends HttpRoutesWordSpec {

  val testApiKey   = "test-api-key-12345"
  val apiConfig    = ApiConfig(testApiKey)
  val apiKeyHeader = Header.Raw(CIString("X-API-Key"), testApiKey)

  "A SecurityController" when {
    "GET /securities/:ticker" should {
      "return 200 and security on success" in {
        val svc = mocks
        when(svc.find(any[Ticker])).thenReturnIO(AAPLSecurity)

        val res = for
          controller <- SecurityController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/securities/aapl", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val resBody = s"""{
                        |  "ticker" : "AAPL",
                        |  "exchange" : "nasdaq",
                        |  "name" : "Apple Inc.",
                        |  "kind" : "stock",
                        |  "isActive" : true,
                        |  "exclusionReason" : null,
                        |  "listingStatus" : "listed",
                        |  "companyProfileLastUpdatedAt" : "2024-01-15T10:00:00Z"
                        |}""".stripMargin
        res mustHaveStatus (Status.Ok, Some(resBody))
        verify(svc).find(AAPL)
      }

      "return 404 when security is not found" in {
        val svc    = mocks
        val ticker = Ticker("UNKNOWN")
        when(svc.find(ticker)).thenRaiseError(AppError.SecurityNotFound(ticker))

        val res = for
          controller <- SecurityController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/securities/unknown", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val resBody = s"""{"message" : "Could not find security for UNKNOWN"}""".stripMargin
        res mustHaveStatus (Status.NotFound, Some(resBody))
        verify(svc).find(ticker)
      }

    }

    "GET /securities/exchange/:exchange" should {
      "return 200 and list of securities on success" in {
        val svc = mocks
        when(svc.findByExchange(any[Exchange])).thenReturnIO(List(AAPLSecurity, MSFTSecurity))

        val res = for
          controller <- SecurityController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/securities/exchange/nasdaq", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val resBody =
          s"""[
             |  {
             |    "ticker" : "AAPL",
             |    "exchange" : "nasdaq",
             |    "name" : "Apple Inc.",
             |    "kind" : "stock",
             |    "isActive" : true,
             |    "exclusionReason" : null,
             |    "listingStatus" : "listed",
             |    "companyProfileLastUpdatedAt" : "2024-01-15T10:00:00Z"
             |  },
             |  {
             |    "ticker" : "MSFT",
             |    "exchange" : "nasdaq",
             |    "name" : "Microsoft Corporation",
             |    "kind" : "stock",
             |    "isActive" : true,
             |    "exclusionReason" : null,
             |    "listingStatus" : "listed",
             |    "companyProfileLastUpdatedAt" : null
             |  }
             |]""".stripMargin

        res mustHaveStatus (Status.Ok, Some(resBody))
        verify(svc).findByExchange(Exchange.NASDAQ)
      }

    }

    "GET /securities/tickers" should {
      "return the ticker list without interpreting tickers as a ticker" in {
        val svc = mocks
        when(svc.getAllTickers).thenReturnIO(List(AAPL, MSFT))
        val response = for
          controller <- SecurityController.make(svc, apiConfig)
          result     <- controller.routes.orNotFound.run(Request[IO](uri = uri"/securities/tickers").withHeaders(apiKeyHeader))
        yield result
        response mustHaveStatus (Status.Ok, Some("""["AAPL","MSFT"]"""))
        verify(svc).getAllTickers
      }

    }
  }

  def mocks: SecurityService[IO] = mock[SecurityService[IO]]
}
