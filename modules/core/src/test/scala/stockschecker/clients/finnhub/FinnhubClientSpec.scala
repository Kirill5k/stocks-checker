package stockschecker.clients.finnhub

import cats.effect.IO
import kirill5k.common.sttp.test.Sttp4WordSpec
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.{CompanyProfile, Exchange, FinancialMetrics, Security, SecurityKind, Ticker}
import sttp.client4.testing.ResponseStub
import fs2.Stream
import sttp.model.StatusCode

import java.time.LocalDate

class FinnhubClientSpec extends Sttp4WordSpec {
  "A FinnhubClient" when {

    val config = FinnhubClientConfig("http://finnhub.io", "api-key")

    "getListedSecurities" should {
      "return list of securities for NASDAQ on success" in {
        val expectedParams = Map("token" -> "api-key", "exchange" -> "US", "mic" -> "XNAS")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(Stream.emit(readJson("finnhub/list-stocks-success.json")).through(fs2.text.utf8.encode))
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
        val expectedParams = Map("token" -> "api-key", "exchange" -> "US", "mic" -> "XNYS")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/symbol") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(Stream.emit(readJson("finnhub/list-stocks-success.json")).through(fs2.text.utf8.encode))
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
        val expectedParams = Map("token" -> "api-key", "symbol" -> "AAPL")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/profile2") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/company-profile-success.json"))
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
              description = Some(
                "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide."
              ),
              website = "https://www.apple.com/",
              ipoDate = Some(LocalDate.parse("1980-12-12")),
              currency = "USD",
              marketCap = 3989245252568L
            )
          )
        }
      }

      "return None when company profile is not found" in {
        val expectedParams = Map("token" -> "api-key", "symbol" -> "INVALID")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/profile2") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/company-profile-not-found.json"))
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

      "retry on 429 Too Many Requests and succeed on second attempt" in {
        val testingBackend = fs2BackendStub.whenAnyRequest
          .thenRespondCyclic(
            ResponseStub.adjust("""{"error":"API limit exceeded"}""", StatusCode.TooManyRequests),
            ResponseStub.adjust(readJson("finnhub/company-profile-success.json"))
          )

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
              description = Some(
                "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide."
              ),
              website = "https://www.apple.com/",
              ipoDate = Some(LocalDate.parse("1980-12-12")),
              currency = "USD",
              marketCap = 3989245252568L
            )
          )
        }
      }
    }

    "getFinancialMetrics" should {
      "return financial metrics on success" in {
        val expectedParams = Map("token" -> "api-key", "symbol" -> "AAPL", "metric" -> "all")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/metric") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/basic-financials-success.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- FinnhubClient.make[IO](config, testingBackend)
          metrics <- client.getFinancialMetrics(Ticker("AAPL"))
        yield metrics

        result.asserting { m =>
          m mustBe Some(
            FinancialMetrics(
              ticker = Ticker("AAPL"),
              peRatioTtm = Some(BigDecimal(34.1119)),
              epsTtm = Some(BigDecimal("7.459300000000001")),
              roeTtm = Some(BigDecimal(164.05)),
              dividendYieldAnnual = Some(BigDecimal(0.40148234)),
              debtToEquityAnnual = Some(BigDecimal(1.338)),
              profitMarginTtm = Some(BigDecimal(26.92)),
              freeCashFlowPerShareTtm = Some(BigDecimal(6.86253)),
              revenueGrowth5Y = Some(BigDecimal(8.68)),
              epsGrowth5Y = Some(BigDecimal(17.91)),
              priceHigh52Week = Some(BigDecimal(288.62)),
              priceLow52Week = Some(BigDecimal(169.2101))
            )
          )
        }
      }

      "return None when financial metrics are not found" in {
        val expectedParams = Map("token" -> "api-key", "symbol" -> "INVALID", "metric" -> "all")
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v1/stock/metric") && r.hasParams(expectedParams) =>
              ResponseStub.adjust(readJson("finnhub/basic-financials-not-found.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client  <- FinnhubClient.make[IO](config, testingBackend)
          metrics <- client.getFinancialMetrics(Ticker("INVALID"))
        yield metrics

        result.asserting { m =>
          m mustBe None
        }
      }

      "retry on 429 Too Many Requests and succeed on second attempt" in {
        val testingBackend = fs2BackendStub.whenAnyRequest
          .thenRespondCyclic(
            ResponseStub.adjust("""{"error":"API limit exceeded"}""", StatusCode.TooManyRequests),
            ResponseStub.adjust(readJson("finnhub/basic-financials-success.json"))
          )

        val result = for
          client  <- FinnhubClient.make[IO](config, testingBackend)
          profile <- client.getFinancialMetrics(Ticker("AAPL"))
        yield profile

        result.asserting { m =>
          m mustBe Some(
            FinancialMetrics(
              ticker = Ticker("AAPL"),
              peRatioTtm = Some(BigDecimal(34.1119)),
              epsTtm = Some(BigDecimal("7.459300000000001")),
              roeTtm = Some(BigDecimal(164.05)),
              dividendYieldAnnual = Some(BigDecimal(0.40148234)),
              debtToEquityAnnual = Some(BigDecimal(1.338)),
              profitMarginTtm = Some(BigDecimal(26.92)),
              freeCashFlowPerShareTtm = Some(BigDecimal(6.86253)),
              revenueGrowth5Y = Some(BigDecimal(8.68)),
              epsGrowth5Y = Some(BigDecimal(17.91)),
              priceHigh52Week = Some(BigDecimal(288.62)),
              priceLow52Week = Some(BigDecimal(169.2101))
            )
          )
        }
      }
    }
  }
}
