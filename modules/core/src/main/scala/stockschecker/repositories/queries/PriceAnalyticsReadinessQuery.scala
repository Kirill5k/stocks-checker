package stockschecker.repositories.queries

import io.circe.{Codec, Decoder, Encoder}
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import mongo4cats.operations.{Aggregate, Projection, Sort}
import stockschecker.domain.{InvestmentRecipe, PriceAnalyticsReadiness, Ticker}
import stockschecker.repositories.PriceAnalyticsRepository

import java.time.{Instant, LocalDate}

private[repositories] object PriceAnalyticsReadinessQuery extends MongoJsonCodecs:
  final case class Row(
      ticker: Ticker,
      calculatedAt: Option[Instant],
      latestPriceDate: Option[LocalDate],
      latestPrice: Option[BigDecimal]
  ):
    def toDomain(at: Instant, recipe: InvestmentRecipe): PriceAnalyticsReadiness =
      PriceAnalyticsReadiness.persisted(ticker, calculatedAt, latestPriceDate, latestPrice, at, recipe)

  object Row:
    // PricePerformanceSummary stores session dates as ISO strings; the outer updatedAt remains a BSON date.
    private given Codec[LocalDate] = Codec.from(Decoder.decodeLocalDate, Encoder.encodeLocalDate)
    given Codec.AsObject[Row]      = Codec.AsObject.derived[Row]
    given MongoCodecProvider[Row]  = deriveCirceCodecProvider[Row]

  val pipeline: Aggregate =
    val analytics = PriceAnalyticsRepository.Field
    Aggregate
      .project(
        Projection
          .computed("ticker", s"$$${analytics.Id}")
          .computed("calculatedAt", s"$$${analytics.UpdatedAt}")
          .computed("latestPriceDate", s"$$${analytics.PerformanceSummary.Root}.latestPriceDate")
          .computed("latestPrice", s"$$${analytics.PerformanceSummary.LatestPrice}")
          .excludeId
      )
      .sort(Sort.asc("ticker"))
