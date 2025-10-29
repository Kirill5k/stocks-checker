package stockschecker.clients.finnhub

import cats.effect.IO
import kirill5k.common.sttp.test.SttpWordSpec
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.{CompanyProfile, Exchange, Security, SecurityKind, Ticker}
import sttp.client3.Response
import fs2.Stream

import java.time.LocalDate

class FinnhubClientSpec extends SttpWordSpec {
  "A FinnhubClient" when {

    val config = FinnhubClientConfig("http://finnhub.io", "api-key")

    "getListedSecurities" should {
      "return list of securities for NASDAQ on success" in {
        val expectedParams = Map("apikey" -> "api-key", "exchange" -> "US", "mic" -> "XNAS")
        val testingBackend = backendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              Response.ok(Right(Stream.emit(readJson("finnhub/list-stocks-success.json")).through(fs2.text.utf8.encode)))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client     <- FinnhubClient.make[IO](config, testingBackend)
          securities <- client.getListedSecurities(Exchange.NASDAQ).compile.toList
        yield securities

        result.asserting { s =>
          s must have size 2
          s.head mustBe Security(
            ticker = Ticker("RMSGW"),
            exchange = Exchange.NASDAQ,
            name = "REAL MESSENGER CORP-28",
            kind = SecurityKind.Other
          )
          s(1) mustBe Security(
            ticker = Ticker("FOXX"),
            exchange = Exchange.NASDAQ,
            name = "FOXX DEVELOPMENT HOLDINGS IN",
            kind = SecurityKind.Stock
          )
        }
      }

      "return list of securities for NYSE on success" in {
        val expectedParams = Map("apikey" -> "api-key", "exchange" -> "US", "mic" -> "XNYS")
        val testingBackend = backendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              Response.ok(Right(Stream.emit(readJson("finnhub/list-stocks-success.json")).through(fs2.text.utf8.encode)))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client     <- FinnhubClient.make[IO](config, testingBackend)
          securities <- client.getListedSecurities(Exchange.NYSE).compile.toList
        yield securities

        result.asserting { s =>
          s must have size 2
        }
      }
    }

    "getCompanyProfile" should {
      "return company profile on success" in {
        val expectedParams = Map("apikey" -> "api-key", "symbol" -> "AAPL")
        val testingBackend = backendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              Response.ok(readJson("finnhub/company-profile-success.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- FinnhubClient.make[IO](config, testingBackend)
          profile <- client.getCompanyProfile(Ticker("AAPL"))
        yield profile

        result.asserting { cp =>
          cp mustBe Some(
            CompanyProfile(
              ticker = Ticker("AAPL"),
              name = "APPLE INC",
              country = "US",
              industry = "Technology",
              description =
                "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide.",
              website = "https://www.apple.com/",
              ipoDate = LocalDate.parse("1980-12-12"),
              currency = "USD",
              marketCap = 3989245252L
            )
          )
        }
      }

      "return None when company profile is not found" in {
        val expectedParams = Map("apikey" -> "api-key", "symbol" -> "INVALID")
        val testingBackend = backendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              Response.ok(readJson("finnhub/company-profile-not-found.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- FinnhubClient.make[IO](config, testingBackend)
          profile <- client.getCompanyProfile(Ticker("INVALID"))
        yield profile

        result.asserting { cp =>
          cp mustBe None
        }
      }
    }
  }
}
