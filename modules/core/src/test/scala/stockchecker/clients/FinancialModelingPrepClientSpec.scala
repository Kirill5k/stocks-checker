package stockchecker.clients

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.sttp.test.SttpWordSpec
import stockchecker.common.config.FinancialModelingPrepConfig
import stockchecker.domain.{CompanyProfile, Stock, Ticker}
import sttp.client3.{Response, SttpBackend}

import java.time.{Instant, LocalDate}

class FinancialModelingPrepClientSpec extends SttpWordSpec {
  "A FinancialModelingPrepClient" when {

    val time   = Instant.parse("2024-01-01T00:00:00Z")
    val config = FinancialModelingPrepConfig("http://financialmodelingprep.com", "api-key")

    given Clock[IO] = Clock.mock[IO](time)

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
          s.head mustBe Stock(
            ticker = Ticker("PMGOLD.AX"),
            name = "Perth Mint Gold",
            price = BigDecimal(17.94),
            exchange = "Australian Securities Exchange",
            exchangeShortName = "ASX",
            stockType = "etf",
            lastUpdatedAt = time
          )
        }
      }
    }

    "getCompanyProfile" should {
      "return company profile on success" in {
        val testingBackend: SttpBackend[IO, Any] = backendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/api/v3/profile/AAPL") && r.hasParams(Map("apikey" -> "api-key")) =>
              Response.ok(readJson("financial-modeling-prep/company-profile.json"))
            case r => throw new RuntimeException(s"Unhandled request to ${r.uri.toString}")
          }

        val result = for
          client <- FinancialModelingPrepClient.make[IO](config, testingBackend)
          stocks <- client.getCompanyProfile(Ticker("AAPL"))
        yield stocks

        result.asserting { cp =>
          cp mustBe Some(
            CompanyProfile(
              ticker = Ticker("AAPL"),
              name = "Apple Inc.",
              country = "US",
              sector = "Technology",
              industry = "Consumer Electronics",
              description = "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide. The company offers iPhone, a line of smartphones; Mac, a line of personal computers; iPad, a line of multi-purpose tablets; and wearables, home, and accessories comprising AirPods, Apple TV, Apple Watch, Beats products, and HomePod. It also provides AppleCare support and cloud services; and operates various platforms, including the App Store that allow customers to discover and download applications and digital content, such as books, music, video, games, and podcasts. In addition, the company offers various services, such as Apple Arcade, a game subscription service; Apple Fitness+, a personalized fitness service; Apple Music, which offers users a curated listening experience with on-demand radio stations; Apple News+, a subscription news and magazine service; Apple TV+, which offers exclusive original content; Apple Card, a co-branded credit card; and Apple Pay, a cashless payment service, as well as licenses its intellectual property. The company serves consumers, and small and mid-sized businesses; and the education, enterprise, and government markets. It distributes third-party applications for its products through the App Store. The company also sells its products through its retail and online stores, and direct sales force; and third-party cellular network carriers, wholesalers, retailers, and resellers. Apple Inc. was incorporated in 1977 and is headquartered in Cupertino, California.",
              website = "https://www.apple.com",
              ipoDate = LocalDate.parse("1980-12-12"),
              currency = "USD",
              stockPrice = BigDecimal(232.98),
              marketCap = 3572538618000L,
              averageTradedVolume = 69852872L,
              isEtf = false,
              isActivelyTrading = true,
              isFund = false,
              isAdr = false,
              lastUpdatedAt = time
            )
          )
        }
      }
    }
  }
}
