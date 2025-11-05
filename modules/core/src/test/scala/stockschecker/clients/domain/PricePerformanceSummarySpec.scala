package stockschecker.clients.domain

import cats.data.NonEmptyList
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.{PriceCandle, PricePerformanceSummary, Ticker}

import java.time.LocalDate

class PricePerformanceSummarySpec extends AnyWordSpec with Matchers {

  private val ticker = Ticker("AAPL")

  "PricePerformanceSummary.from" when {

    "given a single price candle" should {
      "return summary with no period changes" in {
        val candle = PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 102, 1000000)
        val summary = PricePerformanceSummary.from(ticker, NonEmptyList.one(candle))

        summary.ticker mustBe ticker
        summary.latestPrice mustBe 102
        summary.latestPriceDate mustBe LocalDate.of(2024, 1, 1)
        summary.oneMonthChange mustBe None
        summary.threeMonthChange mustBe None
        summary.sixMonthChange mustBe None
        summary.oneYearChange mustBe None
        summary.threeYearChange mustBe None
        summary.fiveYearChange mustBe None
        summary.tenYearChange mustBe None
        summary.maxChange mustBe None
      }
    }

    "given multiple price candles" should {
      "calculate performance correctly for all available periods" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 12, 1), 100, 105, 95, 200, 1000000), // latest (index 0)
          PriceCandle(LocalDate.of(2024, 11, 1), 100, 105, 95, 190, 1000000), // 1 month ago (index 1)
          PriceCandle(LocalDate.of(2024, 10, 1), 100, 105, 95, 185, 1000000), // index 2
          PriceCandle(LocalDate.of(2024, 9, 1), 100, 105, 95, 160, 1000000),  // 3 months ago (index 3)
          PriceCandle(LocalDate.of(2024, 8, 1), 100, 105, 95, 155, 1000000),  // index 4
          PriceCandle(LocalDate.of(2024, 7, 1), 100, 105, 95, 152, 1000000),  // index 5
          PriceCandle(LocalDate.of(2024, 6, 1), 100, 105, 95, 150, 1000000),  // 6 months ago (index 6)
          PriceCandle(LocalDate.of(2024, 5, 1), 100, 105, 95, 145, 1000000),  // index 7
          PriceCandle(LocalDate.of(2024, 4, 1), 100, 105, 95, 140, 1000000),  // index 8
          PriceCandle(LocalDate.of(2024, 3, 1), 100, 105, 95, 135, 1000000),  // index 9
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 130, 1000000),  // index 10
          PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 125, 1000000),  // index 11
          PriceCandle(LocalDate.of(2023, 12, 1), 100, 105, 95, 100, 1000000)  // 12 months ago (index 12, oldest)
        )

        val summary = PricePerformanceSummary.from(ticker, candles)

        summary.ticker mustBe ticker
        summary.latestPrice mustBe 200
        summary.latestPriceDate mustBe LocalDate.of(2024, 12, 1)
        summary.oneMonthChange mustBe Some(BigDecimal("5.26"))  // (200-190)/190 * 100
        summary.threeMonthChange mustBe Some(BigDecimal("25.00")) // (200-160)/160 * 100
        summary.sixMonthChange mustBe Some(BigDecimal("33.33"))   // (200-150)/150 * 100
        summary.oneYearChange mustBe Some(BigDecimal("100.00"))    // (200-100)/100 * 100
        summary.maxChange mustBe Some(BigDecimal("100.00"))        // (200-100)/100 * 100 (from oldest)
      }

      "calculate exact percentage changes with proper rounding" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 12, 1), 100, 105, 95, BigDecimal("150.75"), 1000000),
          PriceCandle(LocalDate.of(2024, 11, 1), 100, 105, 95, BigDecimal("100.50"), 1000000)
        )

        val summary = PricePerformanceSummary.from(ticker, candles)

        // (150.75 - 100.50) / 100.50 * 100 = 50.00 (rounded to 2 decimal places)
        summary.oneMonthChange mustBe Some(BigDecimal("50.00"))
      }

      "handle zero price gracefully by returning None" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 100, 1000000),
          PriceCandle(LocalDate.of(2024, 1, 1), 0, 0, 0, 0, 1000000) // zero price
        )

        val summary = PricePerformanceSummary.from(ticker, candles)

        summary.oneMonthChange mustBe None
        summary.maxChange mustBe None
      }

      "handle negative price changes correctly" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 80, 1000000),  // latest
          PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 100, 1000000)  // 1 month ago
        )

        val summary = PricePerformanceSummary.from(ticker, candles)

        // (80 - 100) / 100 * 100 = -20.00
        summary.oneMonthChange mustBe Some(BigDecimal("-20.00"))
        summary.maxChange mustBe Some(BigDecimal("-20.00"))
      }

      "return None for periods beyond available history" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 3, 1), 100, 105, 95, 200, 1000000), // index 0
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 190, 1000000), // index 1
          PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 100, 1000000)  // index 2
        )

        val summary = PricePerformanceSummary.from(ticker, candles)

        summary.oneMonthChange mustBe defined // index 1 exists
        summary.threeMonthChange mustBe None // index 3 doesn't exist
        summary.sixMonthChange mustBe None // index 6 doesn't exist
        summary.oneYearChange mustBe None // index 12 doesn't exist
        summary.threeYearChange mustBe None
        summary.fiveYearChange mustBe None
        summary.tenYearChange mustBe None
        summary.maxChange mustBe defined // oldest candle exists
      }

      "calculate maxChange from the oldest candle in tail" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 5, 1), 100, 105, 95, 300, 1000000), // latest
          PriceCandle(LocalDate.of(2024, 4, 1), 100, 105, 95, 250, 1000000),
          PriceCandle(LocalDate.of(2024, 3, 1), 100, 105, 95, 200, 1000000),
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 150, 1000000),
          PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 100, 1000000)  // oldest (last in tail)
        )

        val summary = PricePerformanceSummary.from(ticker, candles)
        
        // maxChange should be calculated from oldest: (300 - 100) / 100 * 100 = 200.00
        summary.maxChange mustBe Some(BigDecimal("200.00"))
      }

      "handle very small percentage changes with proper precision" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, BigDecimal("100.01"), 1000000),
          PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, BigDecimal("100.00"), 1000000)
        )

        val summary = PricePerformanceSummary.from(ticker, candles)
        
        // (100.01 - 100.00) / 100.00 * 100 = 0.01
        summary.oneMonthChange mustBe Some(BigDecimal("0.01"))
      }

      "handle large percentage changes" in {
        val candles = NonEmptyList.of(
          PriceCandle(LocalDate.of(2024, 2, 1), 100, 105, 95, 1000, 1000000),
          PriceCandle(LocalDate.of(2024, 1, 1), 100, 105, 95, 10, 1000000)
        )

        val summary = PricePerformanceSummary.from(ticker, candles)
        
        // (1000 - 10) / 10 * 100 = 9900.00
        summary.oneMonthChange mustBe Some(BigDecimal("9900.00"))
      }
    }
  }
}
