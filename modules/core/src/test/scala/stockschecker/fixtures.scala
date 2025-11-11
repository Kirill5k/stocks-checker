package stockschecker

import cats.data.NonEmptyList
import mongo4cats.bson.ObjectId
import stockschecker.actions.Action
import stockschecker.domain.{Command, CommandId, CompanyProfile, Exchange, PriceCandle, PricePerformanceSummary, Schedule, Security, SecurityKind, Stock, Ticker}

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

object fixtures {

  val ts: Instant = Instant.now.truncatedTo(ChronoUnit.MILLIS)

  val AAPL: Ticker = Ticker("AAPL")
  val MSFT: Ticker = Ticker("MSFT")

  val AAPLSecurity: Security = Security(
    ticker = AAPL,
    exchange = Exchange.NASDAQ,
    name = "Apple Inc.",
    kind = SecurityKind.Stock,
    companyProfileLastUpdatedAt = Some(Instant.parse("2024-01-15T10:00:00Z"))
  )

  val MSFTSecurity: Security = Security(
    ticker = MSFT,
    exchange = Exchange.NASDAQ,
    name = "Microsoft Corporation",
    kind = SecurityKind.Stock
  )

  val AAPLCompanyProfile: CompanyProfile = CompanyProfile(
    ticker = AAPL,
    name = "Apple Inc.",
    country = "US",
    industry = "Consumer Electronics",
    description = Some("Apple Inc. designs, manufactures, and markets smartphones, personal computers, tablets, wearables, and accessories worldwide. The company offers iPhone, a line of smartphones; Mac, a line of personal computers; iPad, a line of multi-purpose tablets; and wearables, home, and accessories comprising AirPods, Apple TV, Apple Watch, Beats products, and HomePod. It also provides AppleCare support and cloud services; and operates various platforms, including the App Store that allow customers to discover and download applications and digital content, such as books, music, video, games, and podcasts. In addition, the company offers various services, such as Apple Arcade, a game subscription service; Apple Fitness+, a personalized fitness service; Apple Music, which offers users a curated listening experience with on-demand radio stations; Apple News+, a subscription news and magazine service; Apple TV+, which offers exclusive original content; Apple Card, a co-branded credit card; and Apple Pay, a cashless payment service, as well as licenses its intellectual property. The company serves consumers, and small and mid-sized businesses; and the education, enterprise, and government markets. It distributes third-party applications for its products through the App Store. The company also sells its products through its retail and online stores, and direct sales force; and third-party cellular network carriers, wholesalers, retailers, and resellers. Apple Inc. was incorporated in 1977 and is headquartered in Cupertino, California."),
    website = "https://www.apple.com",
    ipoDate = LocalDate.parse("1980-12-12"),
    currency = "USD",
    marketCap = 3439591971000L
  )

  val MSFTCompanyProfile: CompanyProfile = CompanyProfile(
    ticker = MSFT,
    name = "Microsoft Corporation",
    country = "US",
    industry = "Software—Infrastructure",
    description = Some("Microsoft Corporation develops, licenses, and supports software, services, devices, and solutions worldwide."),
    website = "https://www.microsoft.com",
    ipoDate = LocalDate.parse("1986-03-13"),
    currency = "USD",
    marketCap = 3100000000000L
  )

  val AAPLPriceCandles: NonEmptyList[PriceCandle] = NonEmptyList.of(
    PriceCandle(
      date = LocalDate.parse("2025-10-01"),
      open = BigDecimal("225.00"),
      high = BigDecimal("230.00"),
      low = BigDecimal("220.00"),
      close = BigDecimal("228.50"),
      volume = 50000000L
    ),
    PriceCandle(
      date = LocalDate.parse("2025-09-01"),
      open = BigDecimal("220.00"),
      high = BigDecimal("225.00"),
      low = BigDecimal("215.00"),
      close = BigDecimal("223.00"),
      volume = 48000000L
    ),
    PriceCandle(
      date = LocalDate.parse("2025-08-01"),
      open = BigDecimal("210.00"),
      high = BigDecimal("220.00"),
      low = BigDecimal("205.00"),
      close = BigDecimal("218.00"),
      volume = 52000000L
    )
  )

  val AAPLPricePerformanceSummary: PricePerformanceSummary = PricePerformanceSummary.from(AAPL, AAPLPriceCandles)

  val FetchLatestSecuritiesCommand: Command = Command(
    id = CommandId(ObjectId.gen),
    isActive = true,
    action = Action.DiscoverSecurities(NonEmptyList.of(Exchange.NASDAQ)),
    schedule = Schedule.Periodic(20.minutes),
    lastExecutedAt = Some(ts),
    executionCount = 1,
    maxExecutions = Some(10)
  )

  val AAPLStock: Stock = Stock(
    security = AAPLSecurity,
    profile = Some(AAPLCompanyProfile),
    performanceSummary = Some(AAPLPricePerformanceSummary)
  )

  val MSFTStock: Stock = Stock(
    security = MSFTSecurity,
    profile = Some(MSFTCompanyProfile),
    performanceSummary = None
  )
}
