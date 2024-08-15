package stockschecker.repositories

import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import stockschecker.actions.Action
import stockschecker.domain.{Command, CommandId, CompanyProfile, CreateCommand, Schedule, Stock, Ticker}

import java.time.{Instant, LocalDate}

private[repositories] object entities extends MongoJsonCodecs {

  final case class StockEntity(
      _id: String,
      ticker: Ticker,
      name: String,
      price: BigDecimal,
      exchange: String,
      exchangeShortName: String,
      stockType: String,
      lastUpdatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: Stock =
      Stock(
        ticker = ticker,
        name = name,
        price = price,
        exchange = exchange,
        exchangeShortName = exchangeShortName,
        stockType = stockType,
        lastUpdatedAt = lastUpdatedAt
      )

  object StockEntity:
    given MongoCodecProvider[StockEntity] = deriveCirceCodecProvider[StockEntity]
    def from(stock: Stock): StockEntity =
      StockEntity(
        _id = s"${stock.ticker}${stock.lastUpdatedAt.toString.substring(0, 10)}",
        ticker = stock.ticker,
        name = stock.name,
        price = stock.price,
        exchange = stock.exchange,
        exchangeShortName = stock.exchangeShortName,
        stockType = stock.stockType,
        lastUpdatedAt = stock.lastUpdatedAt
      )

  final case class CompanyProfileEntity(
      _id: Ticker,
      name: String,
      country: String,
      sector: String,
      industry: String,
      description: String,
      website: String,
      ipoDate: LocalDate,
      currency: String,
      stockPrice: BigDecimal,
      marketCap: Long,
      averageTradedVolume: Long,
      isEtf: Boolean,
      isActivelyTrading: Boolean,
      isFund: Boolean,
      isAdr: Boolean,
      lastUpdatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: CompanyProfile =
      CompanyProfile(
        ticker = _id,
        name = name,
        country = country,
        sector = sector,
        industry = industry,
        description = description,
        website = website,
        ipoDate = ipoDate,
        currency = currency,
        stockPrice = stockPrice,
        marketCap = marketCap,
        averageTradedVolume = averageTradedVolume,
        isEtf = isEtf,
        isActivelyTrading = isActivelyTrading,
        isFund = isFund,
        isAdr = isAdr,
        lastUpdatedAt = lastUpdatedAt
      )

  object CompanyProfileEntity:
    given MongoCodecProvider[CompanyProfileEntity] = deriveCirceCodecProvider[CompanyProfileEntity]
    def from(profile: CompanyProfile): CompanyProfileEntity =
      CompanyProfileEntity(
        _id = profile.ticker,
        name = profile.name,
        country = profile.country,
        sector = profile.sector,
        industry = profile.industry,
        description = profile.description,
        website = profile.website,
        ipoDate = profile.ipoDate,
        currency = profile.currency,
        stockPrice = profile.stockPrice,
        marketCap = profile.marketCap,
        averageTradedVolume = profile.averageTradedVolume,
        isEtf = profile.isEtf,
        isActivelyTrading = profile.isActivelyTrading,
        isFund = profile.isFund,
        isAdr = profile.isAdr,
        lastUpdatedAt = profile.lastUpdatedAt
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
