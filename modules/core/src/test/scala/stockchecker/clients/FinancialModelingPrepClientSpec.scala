package stockchecker.clients

import cats.effect.IO
import kirill5k.common.sttp.test.SttpWordSpec
import stockchecker.common.config.FinancialModelingPrepConfig
import stockchecker.domain.{Symbol, Ticker}
import sttp.client3.{Response, SttpBackend}

class FinancialModelingPrepClientSpec extends SttpWordSpec {
  "A FinancialModelingPrepClient" when {

    val config = FinancialModelingPrepConfig("http://financialmodelingprep.com", "api-key")

    "getAllTradedStocks" should {
      "return list of all traded stocks on success" in {
        val testingBackend: SttpBackend[IO, Any] = backendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v3/available-traded/list") && r.hasParams(Map("apikey" -> "api-key")) =>
              Response.ok(readJson("financial-modeling-prep/stock-list.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client <- FinancialModelingPrepClient.make[IO](config, testingBackend)
          stocks <- client.getAllTradedStocks
        yield stocks

        result.asserting { s =>
          s must have size 4
          s.head mustBe Ticker(
            symbol = Symbol("PMGOLD.AX"),
            name = "Perth Mint Gold",
            price = BigDecimal(17.94),
            exchange = "Australian Securities Exchange",
            exchangeShortName = "ASX",
            stockType = "etf"
          )
        }
      }
    }
  }
}
