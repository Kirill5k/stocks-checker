package stockschecker.repositories

import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import stockschecker.actions.Action
import stockschecker.domain.{
  Command,
  CommandId,
  CompanyProfile,
  CreateCommand,
  Exchange,
  FinancialMetrics,
  LatestPrice,
  PriceAnalytics,
  PricePerformanceSummary,
  Schedule,
  Security,
  SecurityKind,
  Stock,
  StockAnalysisMetrics,
  StockAnalysisScores,
  Ticker
}

import java.time.{Instant, LocalDate}

private[repositories] object entities extends MongoJsonCodecs {

  final case class Entity(_id: Ticker) derives Codec.AsObject

  final case class SecurityEntity(
      _id: Ticker,
      ticker: Ticker,
      exchange: Exchange,
      name: String,
      kind: SecurityKind,
      isActive: Boolean,
      companyProfileLastUpdatedAt: Option[Instant],
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: Security =
      Security(
        ticker = _id,
        exchange = exchange,
        name = name,
        kind = kind,
        isActive = isActive,
        companyProfileLastUpdatedAt = companyProfileLastUpdatedAt
      )

  object SecurityEntity:
    given MongoCodecProvider[SecurityEntity]                   = deriveCirceCodecProvider[SecurityEntity]
    def from(security: Security, now: Instant): SecurityEntity =
      SecurityEntity(
        _id = security.ticker,
        ticker = security.ticker,
        exchange = security.exchange,
        name = security.name,
        kind = security.kind,
        isActive = security.isActive,
        companyProfileLastUpdatedAt = security.companyProfileLastUpdatedAt,
        createdAt = now,
        updatedAt = now
      )

  final case class CompanyProfileEntity(
      _id: Ticker,
      name: String,
      country: String,
      industry: String,
      description: Option[String],
      website: String,
      ipoDate: Option[LocalDate],
      currency: String,
      marketCap: Long,
      priceAnalyticsLastUpdatedAt: Option[Instant],
      financialMetricsLastUpdatedAt: Option[Instant],
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: CompanyProfile =
      CompanyProfile(
        ticker = _id,
        name = name,
        country = country,
        industry = industry,
        description = description,
        website = website,
        ipoDate = ipoDate,
        currency = currency,
        marketCap = marketCap,
        priceAnalyticsLastUpdatedAt = priceAnalyticsLastUpdatedAt,
        financialMetricsLastUpdatedAt = financialMetricsLastUpdatedAt
      )

  object CompanyProfileEntity:
    given MongoCodecProvider[CompanyProfileEntity]                        = deriveCirceCodecProvider[CompanyProfileEntity]
    def from(profile: CompanyProfile, now: Instant): CompanyProfileEntity =
      CompanyProfileEntity(
        _id = profile.ticker,
        name = profile.name,
        country = profile.country,
        industry = profile.industry,
        description = profile.description,
        website = profile.website,
        ipoDate = profile.ipoDate,
        currency = profile.currency,
        marketCap = profile.marketCap,
        priceAnalyticsLastUpdatedAt = profile.priceAnalyticsLastUpdatedAt,
        financialMetricsLastUpdatedAt = profile.financialMetricsLastUpdatedAt,
        createdAt = now,
        updatedAt = now
      )

  final case class LatestPriceEntity(
      _id: String,
      ticker: Ticker,
      price: BigDecimal,
      date: LocalDate,
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: LatestPrice =
      LatestPrice(
        ticker = ticker,
        price = price,
        date = date
      )

  object LatestPriceEntity:
    given MongoCodecProvider[LatestPriceEntity] = deriveCirceCodecProvider[LatestPriceEntity]

  final case class PricePerformanceSummaryEntity(
      _id: Ticker,
      latestPrice: BigDecimal,
      latestPriceDate: LocalDate,
      oneMonthChange: Option[BigDecimal],
      threeMonthChange: Option[BigDecimal],
      sixMonthChange: Option[BigDecimal],
      oneYearChange: Option[BigDecimal],
      threeYearChange: Option[BigDecimal],
      fiveYearChange: Option[BigDecimal],
      tenYearChange: Option[BigDecimal],
      maxChange: Option[BigDecimal],
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: PricePerformanceSummary =
      PricePerformanceSummary(
        latestPrice = latestPrice,
        latestPriceDate = latestPriceDate,
        oneMonthChange = oneMonthChange,
        threeMonthChange = threeMonthChange,
        sixMonthChange = sixMonthChange,
        oneYearChange = oneYearChange,
        threeYearChange = threeYearChange,
        fiveYearChange = fiveYearChange,
        tenYearChange = tenYearChange,
        maxChange = maxChange
      )

  object PricePerformanceSummaryEntity:
    given MongoCodecProvider[PricePerformanceSummaryEntity] = deriveCirceCodecProvider[PricePerformanceSummaryEntity]

  final case class PriceAnalyticsEntity(
      _id: Ticker,
      performanceSummary: PricePerformanceSummary,
      metrics: StockAnalysisMetrics,
      scores: StockAnalysisScores,
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: PriceAnalytics =
      PriceAnalytics(
        ticker = _id,
        performanceSummary = performanceSummary,
        metrics = metrics,
        scores = scores
      )

  object PriceAnalyticsEntity:
    given MongoCodecProvider[PriceAnalyticsEntity]                          = deriveCirceCodecProvider[PriceAnalyticsEntity]
    def from(analytics: PriceAnalytics, now: Instant): PriceAnalyticsEntity =
      PriceAnalyticsEntity(
        _id = analytics.ticker,
        performanceSummary = analytics.performanceSummary,
        metrics = analytics.metrics,
        scores = analytics.scores,
        createdAt = now,
        updatedAt = now
      )

  final case class FinancialMetricsEntity(
      _id: Ticker,
      peTTM: Option[BigDecimal],
      epsTTM: Option[BigDecimal],
      roeTTM: Option[BigDecimal],
      dividendYieldIndicatedAnnual: Option[BigDecimal],
      totalDebtToEquityAnnual: Option[BigDecimal],
      netProfitMarginTTM: Option[BigDecimal],
      freeCashFlowPerShareTTM: Option[BigDecimal],
      revenueGrowth5Y: Option[BigDecimal],
      epsGrowth5Y: Option[BigDecimal],
      fiftyTwoWeekHigh: Option[BigDecimal],
      fiftyTwoWeekLow: Option[BigDecimal],
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: FinancialMetrics =
      FinancialMetrics(
        ticker = _id,
        peTTM = peTTM,
        epsTTM = epsTTM,
        roeTTM = roeTTM,
        dividendYieldIndicatedAnnual = dividendYieldIndicatedAnnual,
        totalDebtToEquityAnnual = totalDebtToEquityAnnual,
        netProfitMarginTTM = netProfitMarginTTM,
        freeCashFlowPerShareTTM = freeCashFlowPerShareTTM,
        revenueGrowth5Y = revenueGrowth5Y,
        epsGrowth5Y = epsGrowth5Y,
        fiftyTwoWeekHigh = fiftyTwoWeekHigh,
        fiftyTwoWeekLow = fiftyTwoWeekLow
      )

  object FinancialMetricsEntity:
    given MongoCodecProvider[FinancialMetricsEntity] = deriveCirceCodecProvider[FinancialMetricsEntity]

  final case class CommandEntity(
      _id: ObjectId,
      isActive: Boolean,
      action: Action,
      schedule: Schedule,
      lastExecutedAt: Option[Instant],
      executionCount: Int,
      maxExecutions: Option[Int]
  ) derives Codec.AsObject:
    def toDomain: Command =
      Command(
        id = CommandId(_id),
        isActive = isActive,
        action = action,
        schedule = schedule,
        lastExecutedAt = lastExecutedAt,
        executionCount = executionCount,
        maxExecutions = maxExecutions
      )

  object CommandEntity:
    given MongoCodecProvider[CommandEntity]     = deriveCirceCodecProvider[CommandEntity]
    def from(cmd: CreateCommand): CommandEntity =
      CommandEntity(
        _id = ObjectId.gen,
        isActive = true,
        action = cmd.action,
        schedule = cmd.schedule,
        lastExecutedAt = None,
        executionCount = 0,
        maxExecutions = cmd.maxExecutions
      )

  final case class StockEntity(
      security: SecurityEntity,
      profile: List[CompanyProfileEntity],
      priceAnalytics: List[PriceAnalyticsEntity],
      financialMetrics: List[FinancialMetricsEntity]
  ) derives Codec.AsObject:
    def toDomain: Stock =
      Stock(
        security = security.toDomain,
        profile = profile.headOption.map(_.toDomain),
        priceAnalytics = priceAnalytics.headOption.map(_.toDomain),
        financialMetrics = financialMetrics.headOption.map(_.toDomain)
      )
}
