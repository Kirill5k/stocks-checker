package stockschecker.domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.domain.errors.AppError

import java.time.{Instant, LocalDate}

class PriceAnalyticsAlignmentSpec extends AnyWordSpec with Matchers {
  private val ticker = Ticker("AAA")
  private val today  = LocalDate.parse("2026-10-27")

  private def snapshot(adjustment: PriceAdjustment, dates: List[LocalDate]): PriceHistorySnapshot = {
    val close = if (adjustment == PriceAdjustment.Unadjusted) BigDecimal(100) else BigDecimal(50)
    PriceHistorySnapshot(
      adjustment.value,
      ticker,
      dates.map(date => PriceCandle(date, close, close, close, close, 1L)),
      Provider.TwelveData,
      "USD",
      adjustment,
      Instant.parse("2026-10-27T12:00:00Z"),
      DateRange(today.minusYears(13), today)
    )
  }

  "Price analytics alignment" should {
    "accept a common session exactly seven days old and preserve both observation IDs" in {
      val total  = snapshot(PriceAdjustment.SplitsAndDividends, List(today.minusDays(7)))
      val quoted = snapshot(PriceAdjustment.Unadjusted, List(today.minusDays(7)))
      val result = PriceAnalyticsAlignment.calculate(ticker, total, quoted, today).toOption.get
      result.performanceSummary.latestPriceDate mustBe today.minusDays(7)
      result.performanceSummary.latestPrice mustBe BigDecimal(100)
      result.basis.priceObservationId mustBe Some(total.id)
      result.basis.quotedPriceObservationId mustBe Some(quoted.id)
    }

    "reject a common session eight days old" in {
      val total  = snapshot(PriceAdjustment.SplitsAndDividends, List(today.minusDays(8)))
      val quoted = snapshot(PriceAdjustment.Unadjusted, List(today.minusDays(8)))
      PriceAnalyticsAlignment.calculate(ticker, total, quoted, today) mustBe Left(
        AppError.DataUnavailable(ticker, "the latest matching trading session is more than seven days old")
      )
    }

    "exclude future sessions and align unsorted histories to their latest completed common date" in {
      val total  = snapshot(PriceAdjustment.SplitsAndDividends, List(today.minusDays(2), today.plusDays(1), today.minusDays(1)))
      val quoted = snapshot(PriceAdjustment.Unadjusted, List(today.plusDays(1), today.minusDays(1), today))
      val result = PriceAnalyticsAlignment.calculate(ticker, total, quoted, today).toOption.get
      result.performanceSummary.latestPriceDate mustBe today.minusDays(1)
      result.performanceSummary.latestPrice mustBe BigDecimal(100)
    }
  }
}
