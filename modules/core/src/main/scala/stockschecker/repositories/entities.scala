package stockschecker.repositories

import io.circe.Codec
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import stockschecker.domain.{CompanyProfile, Stock, Ticker}

import java.time.{Instant, LocalDate}

private[repositories] object entities extends MongoJsonCodecs {

  final case class StockEntity(
      _id: Ticker,
      name: String,
      price: BigDecimal,
      exchange: String,
      exchangeShortName: String,
      stockType: String,
      lastUpdatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: Stock =
      Stock(
        ticker = _id,
        name = name,
        price = price,
        exchange = exchange,
        exchangeShortName = exchangeShortName,
        stockType = stockType,
        lastUpdatedAt = lastUpdatedAt
      )

  object StockEntity {
    given MongoCodecProvider[StockEntity] = deriveCirceCodecProvider[StockEntity]

    def from(stock: Stock): StockEntity =
      StockEntity(
        _id = stock.ticker,
        name = stock.name,
        price = stock.price,
        exchange = stock.exchange,
        exchangeShortName = stock.exchangeShortName,
        stockType = stock.stockType,
        lastUpdatedAt = stock.lastUpdatedAt
      )
  }

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

  object CompanyProfileEntity {
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
  }
}
