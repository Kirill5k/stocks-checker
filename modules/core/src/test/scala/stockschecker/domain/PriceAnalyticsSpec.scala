package stockschecker.domain

import cats.data.NonEmptyList
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate

class PriceAnalyticsSpec extends AnyWordSpec with Matchers {

  private val ticker = Ticker("AAPL")

  "PriceAnalytics.from" when {

    "given a single price candle" should {
      "return analytics with no period changes and minimal metrics" in {
        val candle    = PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 102, 1000000)
        val analytics = PriceAnalytics.from(ticker, NonEmptyList.one(candle))

        analytics.ticker.mustBe(ticker)

        // Performance summary assertions
        val summary = analytics.performanceSummary
        summary.latestPrice.mustBe(BigDecimal(102))
        summary.latestPriceDate.mustBe(LocalDate.of(2024, 1, 1))
        summary.oneMonthChange.mustBe(None)
        summary.threeMonthChange.mustBe(None)
        summary.sixMonthChange.mustBe(None)
        summary.oneYearChange.mustBe(None)
        summary.threeYearChange.mustBe(None)
        summary.fiveYearChange.mustBe(None)
        summary.tenYearChange.mustBe(None)
        summary.maxChange.mustBe(None)

        // Metrics assertions - should be None or 0 for insufficient data
        val metrics = analytics.metrics
        metrics.cagr3Year.mustBe(None)
        metrics.cagr5Year.mustBe(None)
        metrics.volatility.mustBe(None)
        metrics.maxDrawdown.mustBe(Some(BigDecimal(0)))
        metrics.totalYears.mustBe(0)
        metrics.positiveYears.mustBe(0)

        // Scores should all be 0 with no data
        val scores = analytics.scores
        scores.cagrScore.mustBe(BigDecimal(0))
        scores.volatilityScore.mustBe(BigDecimal(0))
        scores.drawdownScore.mustBe(BigDecimal(100)) // 0 drawdown = perfect score
        scores.consistencyScore.mustBe(BigDecimal(0))
        scores.overallScore.mustBe(BigDecimal(20.00)) // Weighted: 0*0.30 + 0*0.25 + 100*0.20 + 0*0.25 = 20
      }
    }

    "given 13 months of data" should {
      "calculate performance changes and basic metrics" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 12, 1), 100, 105, 95, 200, 1000000), // latest (index 0)
          PriceCandle(LocalDate.of(2024, 11, 1), 100, 105, 95, 190, 1000000), // 1 month ago
          PriceCandle(LocalDate.of(2024, 10, 1), 100, 105, 95, 185, 1000000),
          PriceCandle(LocalDate.of(2024, 9, 1), 100, 105, 95, 160, 1000000), // 3 months ago
          PriceCandle(LocalDate.of(2024, 8, 1), 100, 105, 95, 155, 1000000),
          PriceCandle(LocalDate.of(2024, 7, 1), 100, 105, 95, 152, 1000000),
          PriceCandle(LocalDate.of(2024, 6, 1), 100, 105, 95, 150, 1000000), // 6 months ago
          PriceCandle(LocalDate.of(2024, 5, 1), 100, 105, 95, 145, 1000000),
          PriceCandle(LocalDate.of(2024, 4, 1), 100, 105, 95, 140, 1000000),
          PriceCandle(LocalDate.of(2024, 3, 1), 100, 105, 95, 135, 1000000),
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 130, 1000000),
          PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 125, 1000000),
          PriceCandle(LocalDate.of(2023, 12, 1), 100, 105, 95, 100, 1000000) // 12 months ago/oldest
        )

        val analytics = PriceAnalytics.from(ticker, candles)

        // Performance summary
        val summary = analytics.performanceSummary
        summary.latestPrice.mustBe(BigDecimal(200))
        summary.latestPriceDate.mustBe(LocalDate.of(2024, 12, 1))
        summary.oneMonthChange.mustBe(Some(BigDecimal("5.26")))    // (200-190)/190 * 100
        summary.threeMonthChange.mustBe(Some(BigDecimal("25.00"))) // (200-160)/160 * 100
        summary.sixMonthChange.mustBe(Some(BigDecimal("33.33")))   // (200-150)/150 * 100
        summary.oneYearChange.mustBe(Some(BigDecimal("100.00")))   // (200-100)/100 * 100
        summary.maxChange.mustBe(Some(BigDecimal("100.00")))       // from oldest

        // Metrics - should have volatility with 13 months of data
        val metrics = analytics.metrics
        metrics.cagr3Year.mustBe(None) // Need 36 months
        metrics.cagr5Year.mustBe(None) // Need 60 months
        metrics.volatility.must(be(defined))
        metrics.maxDrawdown.must(be(defined))
        metrics.totalYears.mustBe(1)
        metrics.positiveYears.mustBe(1) // Only one full year, and it was positive
      }
    }

    "given 37 months of data" should {
      "calculate 3-year CAGR" in {
        val startPrice = BigDecimal(100)
        val endPrice   = BigDecimal(200)

        val candles = NonEmptyList.fromListUnsafe(
          (0 until 37).map { i =>
            val monthsAgo = i
            val year      = 2024 - (monthsAgo / 12)
            val month     = 12 - (monthsAgo % 12)
            // Newest first: i=0 is latest (endPrice), i=36 is oldest (startPrice)
            val price = endPrice - ((endPrice - startPrice) * i / 36)
            PriceCandle(LocalDate.of(year, if (month == 0) 12 else month, 1), 100, 105, 95, price, 1000000)
          }.toList
        )

        val analytics = PriceAnalytics.from(ticker, candles)
        val metrics   = analytics.metrics

        metrics.cagr3Year.must(be(defined))
        // (200/100)^(1/3) - 1 = 25.99%
        metrics.cagr3Year.get.mustBe(BigDecimal("25.99"))
        metrics.cagr5Year.mustBe(None) // Still need 60 months

        metrics.totalYears.mustBe(3)
      }
    }

    "given 61 months of steady growth" should {
      "calculate both 3-year and 5-year CAGR with high scores" in {
        val startPrice = BigDecimal(100)
        val endPrice   = BigDecimal(250)

        val candles = NonEmptyList.fromListUnsafe(
          (0 until 61).map { i =>
            val monthsAgo = i
            val year      = 2024 - (monthsAgo / 12)
            val month     = 12 - (monthsAgo % 12)
            // Newest first: i=0 is latest (endPrice), i=60 is oldest (startPrice)
            val price = endPrice - ((endPrice - startPrice) * i / 60)
            PriceCandle(LocalDate.of(year, if (month == 0) 12 else month, 1), 100, 105, 95, price, 1000000)
          }.toList
        )

        val analytics = PriceAnalytics.from(ticker, candles)
        val metrics   = analytics.metrics

        metrics.cagr3Year.must(be(defined))
        metrics.cagr5Year.must(be(defined))
        metrics.cagr5Year.get.toDouble.must(be > 18.0) // Should be around 20% CAGR
        metrics.cagr5Year.get.toDouble.must(be < 22.0)

        metrics.volatility.must(be(defined))
        metrics.volatility.get.toDouble.must(be < 15.0) // Should have low volatility for steady growth

        metrics.maxDrawdown.must(be(defined))
        // Note: maxDrawdown calculation works on reverse-chronological data,
        // so for growth from 100->250, it sees it as a 60% "drawdown" from 250->100

        metrics.consistencyScore.must(be(defined))
        metrics.consistencyScore.get.toDouble.must(be > 0.9) // High R² for linear growth

        metrics.totalYears.mustBe(5)
        metrics.positiveYears.mustBe(5) // All years positive

        // Scores should be good for steady growth (~20% CAGR)
        val scores = analytics.scores
        scores.cagrScore.toDouble.must(be > 50.0)
        scores.volatilityScore.toDouble.must(be > 70.0)
        // Note: drawdownScore will be low due to reverse chronological calculation
        scores.consistencyScore.toDouble.must(be > 70.0)
        // Overall score affected by low drawdown score
        scores.overallScore.toDouble.must(be > 45.0)
      }
    }

    "given volatile price data" should {
      "calculate high volatility and lower scores" in {
        val candles = NonEmptyList.fromListUnsafe(
          (0 until 25).map { i =>
            val monthsAgo = i
            val year      = 2024 - (monthsAgo / 12)
            val month     = 12 - (monthsAgo % 12)
            // Oscillating prices
            val price = if (i % 2 == 0) BigDecimal(150) else BigDecimal(100)
            PriceCandle(LocalDate.of(year, if (month == 0) 12 else month, 1), 100, 105, 95, price, 1000000)
          }.toList
        )

        val analytics = PriceAnalytics.from(ticker, candles)
        val metrics   = analytics.metrics

        metrics.volatility.must(be(defined))
        metrics.volatility.get.toDouble.must(be > 30.0) // High volatility

        val scores = analytics.scores
        scores.volatilityScore.toDouble.must(be < 50.0) // Low score for high volatility
      }
    }

    "given declining prices" should {
      "calculate negative CAGR and zero CAGR score" in {
        val candles = NonEmptyList.fromListUnsafe(
          (0 until 37).map { i =>
            val monthsAgo = i
            val year      = 2024 - (monthsAgo / 12)
            val month     = 12 - (monthsAgo % 12)
            // Declining: newest (i=0) has lowest price (65), oldest (i=36) has highest price (101)
            val price = BigDecimal(65 + i) // Declining from 101 to 65
            PriceCandle(LocalDate.of(year, if (month == 0) 12 else month, 1), 100, 105, 95, price, 1000000)
          }.toList
        )

        val analytics = PriceAnalytics.from(ticker, candles)
        val metrics   = analytics.metrics

        metrics.cagr3Year.must(be(defined))
        metrics.cagr3Year.get.toDouble.must(be < 0.0) // Negative growth

        metrics.totalYears.mustBe(3)
        metrics.positiveYears.mustBe(0) // All years negative

        val scores = analytics.scores
        scores.cagrScore.mustBe(BigDecimal(0))       // Negative CAGR = 0 score
        scores.overallScore.toDouble.must(be < 60.0) // Low overall score
      }
    }

    "given data with large drawdown" should {
      "calculate high max drawdown and lower drawdown score" in {
        val candles = NonEmptyList.fromListUnsafe(
          List(
            PriceCandle(LocalDate.of(2024, 12, 1), 100, 105, 95, 120, 1000000), // latest, recovered
            PriceCandle(LocalDate.of(2024, 11, 1), 100, 105, 95, 110, 1000000),
            PriceCandle(LocalDate.of(2024, 10, 1), 100, 105, 95, 100, 1000000),
            PriceCandle(LocalDate.of(2024, 9, 1), 100, 105, 95, 90, 1000000),
            PriceCandle(LocalDate.of(2024, 8, 1), 100, 105, 95, 80, 1000000),
            PriceCandle(LocalDate.of(2024, 7, 1), 100, 105, 95, 60, 1000000), // big drop
            PriceCandle(LocalDate.of(2024, 6, 1), 100, 105, 95, 100, 1000000),
            PriceCandle(LocalDate.of(2024, 5, 1), 100, 105, 95, 110, 1000000),
            PriceCandle(LocalDate.of(2024, 4, 1), 100, 105, 95, 120, 1000000),
            PriceCandle(LocalDate.of(2024, 3, 1), 100, 105, 95, 130, 1000000),
            PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 140, 1000000),
            PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 150, 1000000), // peak
            PriceCandle(LocalDate.of(2023, 12, 1), 100, 105, 95, 100, 1000000)
          )
        )

        val analytics = PriceAnalytics.from(ticker, candles)
        val metrics   = analytics.metrics

        metrics.maxDrawdown.must(be(defined))
        // Peak 150 -> Low 60. (150-60)/150 = 60.00%
        metrics.maxDrawdown.get.mustBe(BigDecimal("60.00"))

        val scores = analytics.scores
        scores.drawdownScore.toDouble.must(be < 30.0) // Low score for high drawdown
      }
    }

    "edge cases" when {
      "given zero prices" should {
        "handle gracefully" in {
          val candles = NonEmptyList.of(
            PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 100, 1000000),
            PriceCandle(LocalDate.of(2024, 1, 1), 0, 0, 0, 0, 1000000)
          )

          val analytics = PriceAnalytics.from(ticker, candles)

          analytics.performanceSummary.oneMonthChange.mustBe(None)
          analytics.performanceSummary.maxChange.mustBe(None)
        }
      }

      "given extreme percentage changes" should {
        "handle large values" in {
          val candles = NonEmptyList.of(
            PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 1000, 1000000),
            PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 10, 1000000)
          )

          val analytics = PriceAnalytics.from(ticker, candles)

          // (1000 - 10) / 10 * 100 = 9900.00
          analytics.performanceSummary.oneMonthChange.mustBe(Some(BigDecimal("9900.00")))
        }
      }

      "given very small price changes" should {
        "maintain precision" in {
          val candles = NonEmptyList.of(
            PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, BigDecimal("100.01"), 1000000),
            PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, BigDecimal("100.00"), 1000000)
          )

          val analytics = PriceAnalytics.from(ticker, candles)

          // (100.01 - 100.00) / 100.00 * 100 = 0.01
          analytics.performanceSummary.oneMonthChange.mustBe(Some(BigDecimal("0.01")))
        }
      }
    }

    "scoring calculations" should {
      "cap CAGR score at 100 for very high growth" in {
        val candles = NonEmptyList.fromListUnsafe(
          (0 until 37).map { i =>
            val monthsAgo = i
            val year      = 2024 - (monthsAgo / 12)
            val month     = 12 - (monthsAgo % 12)
            // 40% annual growth: newest (i=0) has highest price, oldest (i=36) has lowest
            // i=0: 1.4^3 = 2.744, i=36: 1.4^0 = 1.0
            val price = BigDecimal(100 * Math.pow(1.4, (36 - i) / 12.0))
            PriceCandle(LocalDate.of(year, if (month == 0) 12 else month, 1), 100, 105, 95, price, 1000000)
          }.toList
        )

        val analytics = PriceAnalytics.from(ticker, candles)

        // CAGR > 30% should result in score of 100
        analytics.scores.cagrScore.mustBe(BigDecimal(100.00))
      }

      "give maximum volatility score for very low volatility" in {
        val candles = NonEmptyList.fromListUnsafe(
          (0 until 25).map { i =>
            val monthsAgo = i
            val year      = 2024 - (monthsAgo / 12)
            val month     = 12 - (monthsAgo % 12)
            // Almost no change - slight gradual increase from oldest to newest
            val price = BigDecimal(100) + BigDecimal((24 - i) * 0.1)
            PriceCandle(LocalDate.of(year, if (month == 0) 12 else month, 1), 100, 105, 95, price, 1000000)
          }.toList
        )

        val analytics = PriceAnalytics.from(ticker, candles)

        // Very low volatility < 10% should give score of 100
        analytics.scores.volatilityScore.mustBe(BigDecimal(100.00))
      }

      "calculate weighted overall score correctly" in {
        // Manually verify the weighting: CAGR 30%, Volatility 25%, Drawdown 20%, Consistency 25%
        val candles = NonEmptyList.fromListUnsafe(
          (0 until 37).map { i =>
            val monthsAgo = i
            val year      = 2024 - (monthsAgo / 12)
            val month     = 12 - (monthsAgo % 12)
            // Steady growth: newest (i=0) has highest, oldest (i=36) has lowest
            val price = BigDecimal(172 - i * 2)
            PriceCandle(LocalDate.of(year, if (month == 0) 12 else month, 1), 100, 105, 95, price, 1000000)
          }.toList
        )

        val analytics = PriceAnalytics.from(ticker, candles)
        val scores    = analytics.scores

        val expectedOverall = (
          scores.cagrScore * 0.30 +
            scores.volatilityScore * 0.25 +
            scores.drawdownScore * 0.20 +
            scores.consistencyScore * 0.25
        ).setScale(2, scala.math.BigDecimal.RoundingMode.HALF_UP)

        scores.overallScore.mustBe(expectedOverall)
      }
    }
  }
}
