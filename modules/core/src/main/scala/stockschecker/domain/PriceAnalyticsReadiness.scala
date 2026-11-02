package stockschecker.domain

import io.circe.Codec

import java.time.{Instant, LocalDate, ZoneOffset}
import java.time.temporal.ChronoUnit

/** Current persisted analytics and their price eligibility; provider retrieval success is reported separately. */
final case class PriceAnalyticsReadiness(
    ticker: Ticker,
    usable: Boolean,
    calculatedAt: Option[Instant],
    latestPriceDate: Option[LocalDate],
    latestPrice: Option[BigDecimal],
    reason: Option[String]
) derives Codec.AsObject

object PriceAnalyticsReadiness:
  def missing(ticker: Ticker): PriceAnalyticsReadiness =
    PriceAnalyticsReadiness(ticker, false, None, None, None, Some("missing-analytics"))

  def persisted(
      ticker: Ticker,
      calculatedAt: Option[Instant],
      latestPriceDate: Option[LocalDate],
      latestPrice: Option[BigDecimal],
      at: Instant,
      recipe: InvestmentRecipe
  ): PriceAnalyticsReadiness =
    val today  = at.atZone(ZoneOffset.UTC).toLocalDate
    val reason =
      if !latestPrice.exists(_ > 0) then Some("invalid-price")
      else
        latestPriceDate match
          case None                                                                        => Some("missing-price-date")
          case Some(date) if date.isAfter(today)                                           => Some("future-price")
          case Some(date) if ChronoUnit.DAYS.between(date, today) > recipe.maxPriceAgeDays => Some("stale-price")
          case _                                                                           => None
    PriceAnalyticsReadiness(ticker, reason.isEmpty, calculatedAt, latestPriceDate, latestPrice, reason)
