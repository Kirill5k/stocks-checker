package stockschecker.domain

import io.circe.Codec as CirceCodec

import java.time.LocalDate

final case class PricePerformanceSummary(
    latestPrice: BigDecimal,
    latestPriceDate: LocalDate,
    oneMonthChange: Option[BigDecimal],
    threeMonthChange: Option[BigDecimal],
    sixMonthChange: Option[BigDecimal],
    oneYearChange: Option[BigDecimal],
    threeYearChange: Option[BigDecimal],
    fiveYearChange: Option[BigDecimal],
    tenYearChange: Option[BigDecimal],
    maxChange: Option[BigDecimal]
) derives CirceCodec.AsObject

final case class StockAnalysisMetrics(
    cagr3Year: Option[BigDecimal],
    cagr5Year: Option[BigDecimal],
    volatility: Option[BigDecimal],
    maxDrawdown: Option[BigDecimal],
    consistencyScore: Option[BigDecimal],
    positiveYears: Int,
    totalYears: Int
) derives CirceCodec.AsObject

final case class StockAnalysisScores(
    overallScore: BigDecimal,
    cagrScore: BigDecimal,
    volatilityScore: BigDecimal,
    drawdownScore: BigDecimal,
    consistencyScore: BigDecimal
) derives CirceCodec.AsObject

final case class PriceAnalytics(
    ticker: Ticker,
    performanceSummary: PricePerformanceSummary,
    metrics: StockAnalysisMetrics,
    scores: StockAnalysisScores,
    basis: Option[PriceAnalyticsBasis] = None
) derives CirceCodec.AsObject

/** Observation IDs identify source vintages. Raw candle retention is bounded, so these IDs do not guarantee replayability. */
final case class PriceAnalyticsBasis(
    purpose: String,
    returnAdjustment: String,
    drawdownFrequency: String,
    riskWindowYears: Int,
    priceObservationId: Option[String],
    quotedPriceObservationId: Option[String]
) derives CirceCodec.AsObject

object PriceAnalytics:
  extension (pa: PriceAnalytics)
    def toLatestPrice: LatestPrice =
      LatestPrice(pa.ticker, pa.performanceSummary.latestPrice, pa.performanceSummary.latestPriceDate)
