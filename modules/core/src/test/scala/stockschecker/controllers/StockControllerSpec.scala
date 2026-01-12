package stockschecker.controllers

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{StockFilters, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.services.StockService
import stockschecker.fixtures.*

class StockControllerSpec extends HttpRoutesWordSpec {

  val testApiKey   = "test-api-key-12345"
  val apiConfig    = ApiConfig(testApiKey)
  val apiKeyHeader = Header.Raw(CIString("X-API-Key"), testApiKey)

  val applStockJson = """
      |{
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
      |"priceAnalytics":{
      | "performanceSummary":{
      |   "latestPrice":228.50,
      |   "latestPriceDate":"2025-10-01",
      |   "oneMonthChange":2.47,
      |   "threeMonthChange":null,
      |   "sixMonthChange":null,
      |   "oneYearChange":null,
      |   "threeYearChange":null,
      |   "fiveYearChange":null,
      |   "tenYearChange":null,
      |   "maxChange":4.82
      | },
      | "metrics":{
      |   "cagr3Year":null,
      |   "cagr5Year":null,
      |   "volatility":null,
      |   "maxDrawdown":0,
      |   "consistencyScore":0.9992,
      |   "positiveYears":0,
      |   "totalYears":0
      | },
      | "scores":{
      |   "overallScore":32.49,
      |   "cagrScore":0,
      |   "volatilityScore":0,
      |   "drawdownScore":100,
      |   "consistencyScore":49.96
      | },
      | "lastUpdatedAt" : null
      |},
      |"financialMetrics":{
      |  "peRatioTtm":34.11,
      |  "epsTtm":7.46,
      |  "roeTtm":164.05,
      |  "dividendYieldAnnual":0.40,
      |  "debtToEquityAnnual":1.34,
      |  "profitMarginTtm":26.92,
      |  "freeCashFlowPerShareTtm":6.86,
      |  "revenueGrowth5Y":8.68,
      |  "epsGrowth5Y":17.91,
      |  "priceHigh52Week":288.62,
      |  "priceLow52Week":169.21,
      |  "lastUpdatedAt":null
      |}
      | }
      |""".stripMargin

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

        res mustHaveStatus (Status.Ok, Some(applStockJson))
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

    "GET /stocks" should {
      "return 200 and all stocks without limit" in {
        val svc = mocks
        when(svc.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock, MSFTStock))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val expectedJson =
          s"""[
            |${applStockJson}
            |,{
            |"ticker":"MSFT",
            |"name":"Microsoft Corporation",
            |"security":{"exchange":"nasdaq","kind":"stock"},
            |"profile":{
            | "country":"US",
            |  "industry":"Software—Infrastructure",
            |  "description":"Microsoft Corporation develops, licenses, and supports software, services, devices, and solutions worldwide.",
            |  "website":"https://www.microsoft.com",
            |  "ipoDate":"1986-03-13",
            |  "currency":"USD",
          |  "marketCap":3100000000000,
          |  "lastUpdatedAt":null
          |  },
            |"priceAnalytics":null,
            |"financialMetrics":{
            |  "peRatioTtm":38.50,
            |  "epsTtm":11.23,
            |  "roeTtm":42.30,
            |  "dividendYieldAnnual":0.75,
            |  "debtToEquityAnnual":0.58,
            |  "profitMarginTtm":36.20,
            |  "freeCashFlowPerShareTtm":9.12,
            |  "revenueGrowth5Y":12.40,
            |  "epsGrowth5Y":15.80,
            |  "priceHigh52Week":420.00,
            |  "priceLow52Week":310.00,
            |  "lastUpdatedAt":null
            |}
            | }]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(expectedJson))
        verify(svc).findAll(StockFilters(), None)
      }

      "return 200 and limited stocks with limit parameter" in {
        val svc = mocks
        when(svc.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks?limit=10", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        val expectedJson = s"""[${applStockJson}]""".stripMargin
        res mustHaveStatus (Status.Ok, Some(expectedJson))
        verify(svc).findAll(StockFilters(), Some(10))
      }

      "return 200 and empty list when no stocks found" in {
        val svc = mocks
        when(svc.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List.empty)

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some("[]"))
        verify(svc).findAll(StockFilters(), None)
      }

      "return 401 when accessed without API key" in {
        val svc = mocks

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks", method = Method.GET)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid API key"}"""))
        verifyNoInteractions(svc)
      }

      "return 200 and filter by exchange" in {
        val svc     = mocks
        when(svc.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks?exchange=nasdaq", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some(s"""[$applStockJson]""".stripMargin))
        verify(svc).findAll(StockFilters(exchange = Some(stockschecker.domain.Exchange.NASDAQ)), None)
      }

      "return 200 and filter by kind" in {
        val svc = mocks
        when(svc.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks?kind=stock", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some(s"""[$applStockJson]""".stripMargin))
        verify(svc).findAll(StockFilters(kind = Some(stockschecker.domain.SecurityKind.Stock)), None)
      }

      "return 200 and filter by minMarketCap" in {
        val svc = mocks
        when(svc.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks?minMarketCap=1000000000000", method = Method.GET).withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some(s"""[${applStockJson}]""".stripMargin))
        verify(svc).findAll(StockFilters(minMarketCap = Some(1000000000000L)), None)
      }

      "return 200 and filter by multiple parameters" in {
        val svc = mocks
        when(svc.findAll(any[StockFilters], anyOpt[Int])).thenReturnIO(List(AAPLStock))

        val res = for
          controller <- StockController.make(svc, apiConfig)
          req = Request[IO](uri = uri"/stocks?exchange=nasdaq&kind=stock&minMarketCap=1000000000000&limit=10", method = Method.GET)
            .withHeaders(apiKeyHeader)
          res <- controller.routes.orNotFound.run(req)
        yield res

        res mustHaveStatus (Status.Ok, Some(s"""[${applStockJson}]""".stripMargin))
        verify(svc).findAll(
          StockFilters(
            exchange = Some(stockschecker.domain.Exchange.NASDAQ),
            kind = Some(stockschecker.domain.SecurityKind.Stock),
            minMarketCap = Some(1000000000000L)
          ),
          Some(10)
        )
      }
    }
  }

  def mocks: StockService[IO] = mock[StockService[IO]]
}
