package stockschecker.domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{Instant, LocalDate}

class PriceAnalyticsReadinessSpec extends AnyWordSpec with Matchers:
  private val ticker = Ticker("AAA")
  private val now = Instant.parse("2026-11-02T00:30:00Z")
  private val today = LocalDate.parse("2026-11-02")
  private val recipe = InvestmentRecipe()

  private def readiness(date: Option[LocalDate], price: Option[BigDecimal] = Some(BigDecimal(100))): PriceAnalyticsReadiness =
    PriceAnalyticsReadiness.persisted(ticker, Some(now), date, price, now, recipe)

  "Price analytics readiness" should {
    "describe absent analytics independently of provider availability" in {
      PriceAnalyticsReadiness.missing(ticker) mustBe
        PriceAnalyticsReadiness(ticker, false, None, None, None, Some("missing-analytics"))
    }

    "accept positive prices through the configured UTC freshness boundary" in {
      readiness(Some(today)).usable mustBe true
      val boundary = readiness(Some(today.minusDays(recipe.maxPriceAgeDays)))
      boundary.usable mustBe true
      boundary.reason mustBe None
      boundary.calculatedAt mustBe Some(now)
      boundary.latestPriceDate mustBe Some(today.minusDays(recipe.maxPriceAgeDays))
      boundary.latestPrice mustBe Some(BigDecimal(100))
    }

    "reject stale and future sessions even when analytics were just calculated" in {
      val stale = readiness(Some(today.minusDays(recipe.maxPriceAgeDays + 1)))
      stale.usable mustBe false
      stale.reason mustBe Some("stale-price")
      val future = readiness(Some(today.plusDays(1)))
      future.usable mustBe false
      future.reason mustBe Some("future-price")
    }

    "reject missing dates and missing or nonpositive prices" in {
      readiness(None).reason mustBe Some("missing-price-date")
      List(None, Some(BigDecimal(0)), Some(BigDecimal(-1))).foreach { price =>
        val result = readiness(Some(today), price)
        result.usable mustBe false
        result.reason mustBe Some("invalid-price")
      }
    }

    "use the current recipe without treating calculation time as price freshness" in {
      val result = PriceAnalyticsReadiness.persisted(
        ticker,
        Some(now.minusSeconds(30L * 86400)),
        Some(today.minusDays(3)),
        Some(BigDecimal(100)),
        now,
        recipe.copy(maxPriceAgeDays = 2)
      )
      result.usable mustBe false
      result.reason mustBe Some("stale-price")
      PriceAnalyticsReadiness.persisted(ticker, None, Some(today), Some(BigDecimal(100)), now, recipe).usable mustBe true
    }
  }
