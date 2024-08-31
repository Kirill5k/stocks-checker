package stockschecker

import mongo4cats.bson.ObjectId
import stockschecker.actions.Action.FetchLatestStocks
import stockschecker.domain.{Command, CommandId, CompanyProfile, Schedule, Stock, Ticker}

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

object fixtures {

  val ts = Instant.now.truncatedTo(ChronoUnit.MILLIS)

  val AAPL = Ticker("AAPL")
  val MSFT = Ticker("MSFT")

  val AAPLStock = Stock(
    ticker = AAPL,
    price = BigDecimal(234.4),
    stockType = "stock",
    lastUpdatedAt = ts
  )

  val MSFTStock = Stock(
    ticker = MSFT,
    price = BigDecimal(449.52),
    stockType = "stock",
    lastUpdatedAt = ts
  )

  val AAPLCompanyProfile = CompanyProfile(
    ticker = AAPL,
    name = "Apple Inc.",
    country = "US",
    sector = "Technology",
    industry = "Consumer Electronics",
    description =
      "Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide. The company offers iPhone, a line of smartphones; Mac, a line of personal computers; iPad, a line of multi-purpose tablets; and wearables, home, and accessories comprising AirPods, Apple TV, Apple Watch, Beats products, and HomePod. It also provides AppleCare support and cloud services; and operates various platforms, including the App Store that allow customers to discover and download applications and digital content, such as books, music, video, games, and podcasts. In addition, the company offers various services, such as Apple Arcade, a game subscription service; Apple Fitness+, a personalized fitness service; Apple Music, which offers users a curated listening experience with on-demand radio stations; Apple News+, a subscription news and magazine service; Apple TV+, which offers exclusive original content; Apple Card, a co-branded credit card; and Apple Pay, a cashless payment service, as well as licenses its intellectual property. The company serves consumers, and small and mid-sized businesses; and the education, enterprise, and government markets. It distributes third-party applications for its products through the App Store. The company also sells its products through its retail and online stores, and direct sales force; and third-party cellular network carriers, wholesalers, retailers, and resellers. Apple Inc. was incorporated in 1977 and is headquartered in Cupertino, California.",
    website = "https://www.apple.com",
    ipoDate = LocalDate.parse("1980-12-12"),
    currency = "USD",
    marketCap = 3439591971000L,
    averageTradedVolume = 68274858L,
    isEtf = false,
    isActivelyTrading = true,
    isFund = false,
    isAdr = false,
    lastUpdatedAt = ts
  )

  val FetchLatestStocksCommand = Command(
    id = CommandId(ObjectId.gen),
    isActive = true,
    action = FetchLatestStocks,
    schedule = Schedule.Periodic(20.minutes),
    lastExecutedAt = Some(ts),
    executionCount = 1,
    maxExecutions = Some(10)
  )
}
