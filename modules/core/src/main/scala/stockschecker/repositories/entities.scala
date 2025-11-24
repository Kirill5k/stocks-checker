package stockschecker.repositories

import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import stockschecker.actions.Action
import stockschecker.domain.{Command, CommandId, CompanyProfile, CreateCommand, Exchange, LatestPrice, PricePerformanceSummary, Schedule, Security, SecurityKind, Stock, Ticker}

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
      ipoDate: LocalDate,
      currency: String,
      marketCap: Long,
      pricePerformanceLastUpdatedAt: Option[Instant],
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
        pricePerformanceLastUpdatedAt = pricePerformanceLastUpdatedAt
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
        pricePerformanceLastUpdatedAt = profile.pricePerformanceLastUpdatedAt,
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
    given MongoCodecProvider[LatestPriceEntity]                 = deriveCirceCodecProvider[LatestPriceEntity]
    def from(latestPrice: LatestPrice, now: Instant): LatestPriceEntity =
      LatestPriceEntity(
        _id = s"${latestPrice.ticker.value}-${latestPrice.date}",
        ticker = latestPrice.ticker,
        price = latestPrice.price,
        date = latestPrice.date,
        createdAt = now,
        updatedAt = now
      )

  final case class PricePerformanceSummaryEntity(
      _id: Ticker,
      ticker: Ticker,
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
        ticker = ticker,
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
    def from(summary: PricePerformanceSummary, now: Instant): PricePerformanceSummaryEntity =
      PricePerformanceSummaryEntity(
        _id = summary.ticker,
        ticker = summary.ticker,
        latestPrice = summary.latestPrice,
        latestPriceDate = summary.latestPriceDate,
        oneMonthChange = summary.oneMonthChange,
        threeMonthChange = summary.threeMonthChange,
        sixMonthChange = summary.sixMonthChange,
        oneYearChange = summary.oneYearChange,
        threeYearChange = summary.threeYearChange,
        fiveYearChange = summary.fiveYearChange,
        tenYearChange = summary.tenYearChange,
        maxChange = summary.maxChange,
        createdAt = now,
        updatedAt = now
      )

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
    given MongoCodecProvider[CommandEntity] = deriveCirceCodecProvider[CommandEntity]
    def from(cmd: Command): CommandEntity   =
      CommandEntity(
        _id = cmd.id.toObjectId,
        isActive = cmd.isActive,
        action = cmd.action,
        schedule = cmd.schedule,
        lastExecutedAt = cmd.lastExecutedAt,
        executionCount = cmd.executionCount,
        maxExecutions = cmd.maxExecutions
      )
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
      performanceSummary: List[PricePerformanceSummaryEntity]
  ) derives Codec.AsObject:
    def toDomain: Stock =
      Stock(
        security = security.toDomain,
        profile = profile.headOption.map(_.toDomain),
        performanceSummary = performanceSummary.headOption.map(_.toDomain)
      )
}
