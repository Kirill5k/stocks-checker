package stockschecker.repositories

import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import stockschecker.actions.Action
import stockschecker.domain.{Command, CommandId, CompanyProfile, CreateCommand, Schedule, SecurityType, StockQuote, Ticker}

import java.time.{Instant, LocalDate}

private[repositories] object entities extends MongoJsonCodecs {

  final case class StockQuoteEntity(
      _id: String,
      ticker: Ticker,
      price: BigDecimal,
      quotedAt: Instant,
      previousClose: Option[BigDecimal] = None,
      changeAmount: Option[BigDecimal] = None,
      changePercent: Option[BigDecimal] = None,
      volume: Option[Long] = None,
      dayHigh: Option[BigDecimal] = None,
      dayLow: Option[BigDecimal] = None,
      createdAt: Instant
  ) derives Codec.AsObject:
    def toDomain: StockQuote =
      StockQuote(
        ticker = ticker,
        price = price,
        quotedAt = quotedAt,
        previousClose = previousClose,
        changeAmount = changeAmount,
        changePercent = changePercent,
        volume = volume,
        dayHigh = dayHigh,
        dayLow = dayLow,
        createdAt = createdAt
      )

  object StockQuoteEntity:
    given MongoCodecProvider[StockQuoteEntity] = deriveCirceCodecProvider[StockQuoteEntity]
    def from(quote: StockQuote): StockQuoteEntity =
      StockQuoteEntity(
        _id = s"${quote.ticker}.${quote.quotedAt.toString.substring(0, 10)}",
        ticker = quote.ticker,
        price = quote.price,
        quotedAt = quote.quotedAt,
        previousClose = quote.previousClose,
        changeAmount = quote.changeAmount,
        changePercent = quote.changePercent,
        volume = quote.volume,
        dayHigh = quote.dayHigh,
        dayLow = quote.dayLow,
        createdAt = quote.createdAt
      )

  final case class CompanyProfileEntity(
      _id: Ticker,
      name: String,
      securityType: String,
      exchange: Option[String],
      currency: String,
      country: Option[String],
      sector: Option[String],
      industry: Option[String],
      description: Option[String],
      website: Option[String],
      ipoDate: Option[LocalDate],
      ceo: Option[String],
      employees: Option[Long],
      marketCap: Option[Long],
      averageVolume: Option[Long],
      week52High: Option[BigDecimal],
      week52Low: Option[BigDecimal],
      isActivelyTrading: Boolean,
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: CompanyProfile =
      CompanyProfile(
        ticker = _id,
        name = name,
        securityType = SecurityType.fromString(securityType),
        exchange = exchange,
        currency = currency,
        country = country,
        sector = sector,
        industry = industry,
        description = description,
        website = website,
        ipoDate = ipoDate,
        ceo = ceo,
        employees = employees,
        marketCap = marketCap,
        averageVolume = averageVolume,
        week52High = week52High,
        week52Low = week52Low,
        isActivelyTrading = isActivelyTrading,
        createdAt = createdAt,
        updatedAt = updatedAt
      )

  object CompanyProfileEntity:
    given MongoCodecProvider[CompanyProfileEntity] = deriveCirceCodecProvider[CompanyProfileEntity]
    def from(profile: CompanyProfile): CompanyProfileEntity =
      CompanyProfileEntity(
        _id = profile.ticker,
        name = profile.name,
        securityType = profile.securityType.value,
        exchange = profile.exchange,
        currency = profile.currency,
        country = profile.country,
        sector = profile.sector,
        industry = profile.industry,
        description = profile.description,
        website = profile.website,
        ipoDate = profile.ipoDate,
        ceo = profile.ceo,
        employees = profile.employees,
        marketCap = profile.marketCap,
        averageVolume = profile.averageVolume,
        week52High = profile.week52High,
        week52Low = profile.week52Low,
        isActivelyTrading = profile.isActivelyTrading,
        createdAt = profile.createdAt,
        updatedAt = profile.updatedAt
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
    def from(cmd: Command): CommandEntity =
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

}
