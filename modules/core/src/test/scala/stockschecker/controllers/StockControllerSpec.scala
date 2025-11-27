package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.Ticker
import stockschecker.domain.errors.AppError
import stockschecker.services.StockService
import stockschecker.fixtures.*

class StockControllerSpec extends HttpRoutesWordSpec {

  val testApiKey   = "test-api-key-12345"
  val apiConfig    = ApiConfig(testApiKey)
  val apiKeyHeader = Header.Raw(CIString("X-API-Key"), testApiKey)

  "A StockController" when {
    "GET /stocks/:ticker" should {
      "return 200 and stock on success" in {
        val svc = mocks
        when(svc.findByTicker(any[Ticker])).thenReturnIO(AAPLStock)

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks/aapl", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val expectedJson =
          """{
            |"ticker":"AAPL",
            |"name":"Apple Inc.",
            |"security":{"exchange":"nasdaq","kind":"stock"},
            |"profile":{
            | "country":"US",
            |  "industry":"Consumer Electronics",
            |  "description":"Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide. The company offers iPhone, a line of smartphones; Mac, a line of personal computers; iPad, a line of multi-purpose tablets; and wearables, home, and accessories comprising AirPods, Apple TV, Apple Watch, Beats products, and HomePod. It also provides AppleCare support and cloud services; and operates various platforms, including the App Store that allow customers to discover and download applications and digital content, such as books, music, video, games, and podcasts. In addition, the company offers various services, such as Apple Arcade, a game subscription service; Apple Fitness+, a personalized fitness service; Apple Music, which offers users a curated listening experience with on-demand radio stations; Apple News+, a subscription news and magazine service; Apple TV+, which offers exclusive original content; Apple Card, a co-branded credit card; and Apple Pay, a cashless payment service, as well as licenses its intellectual property. The company serves consumers, and small and mid-sized businesses; and the education, enterprise, and government markets. It distributes third-party applications for its products through the App Store. The company also sells its products through its retail and online stores, and direct sales force; and third-party cellular network carriers, wholesalers, retailers, and resellers. Apple Inc. was incorporated in 1977 and is headquartered in Cupertino, California.",
            |  "website":"https://www.apple.com",
            |  "ipoDate":"1980-12-12",
            |  "currency":"USD",
            |  "marketCap":3439591971000,
            |  "lastUpdatedAt":"2024-01-15T10:00:00Z"
          |  },
            |"performanceSummary":{
            | "latestPrice":228.50,
            | "latestPriceDate":"2025-10-01",
            | "oneMonthChange":2.47,
            | "threeMonthChange":null,
            | "sixMonthChange":null,
            | "oneYearChange":null,"threeYearChange":null,
            | "fiveYearChange":null,
            | "tenYearChange":null,
            | "maxChange":4.82,
            | "lastUpdatedAt":null}
            | }""".stripMargin
        res mustHaveStatus (Status.Ok, Some(expectedJson))
        verify(svc).findByTicker(AAPL)
      }

      "return 404 on not found" in {
        val svc = mocks
        when(svc.findByTicker(any[Ticker])).thenRaiseError(AppError.SecurityNotFound(Ticker("UNKNOWN")))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks/UNKNOWN", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.NotFound, Some("""{"message":"Could not find security for UNKNOWN"}"""))
        verify(svc).findByTicker(Ticker("UNKNOWN"))
      }
    }
  }

  def mocks: StockService[IO] = mock[StockService[IO]]
}
