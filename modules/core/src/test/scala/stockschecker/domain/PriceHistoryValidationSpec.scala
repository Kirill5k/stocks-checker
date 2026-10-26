package stockschecker.domain

import cats.data.NonEmptyList
import java.time.LocalDate
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class PriceHistoryValidationSpec extends AnyWordSpec with Matchers {
  private def bar(date: String, close: Int) = PriceCandle(LocalDate.parse(date), close, close, close, close, 1L)

  "Price history validation" should {
    "sort by date and reject duplicate, nonpositive and missing-month inputs" in {
      val bars = NonEmptyList.of(bar("2026-08-31", 100), bar("2026-09-30", 110))
      PriceAnalytics.validate(bars).toOption.get.head.date mustBe LocalDate.parse("2026-09-30")
      PriceAnalytics.validate(bars.concatNel(NonEmptyList.one(bars.head))).isLeft mustBe true
      PriceAnalytics.validate(NonEmptyList.of(bar("2026-09-30", 0))).isLeft mustBe true
      PriceAnalytics.validate(NonEmptyList.of(bar("2026-09-30", 110), bar("2026-07-31", 100))).isLeft mustBe true
    }
    "capture a daily drawdown hidden by month-end observations and label its basis" in {
      val daily     = NonEmptyList.of(bar("2026-10-01", 100), bar("2026-10-02", 50), bar("2026-10-05", 100))
      val analytics = PriceAnalytics.fromDaily(Ticker("TEST"), daily)
      analytics.metrics.maxDrawdown mustBe Some(BigDecimal(50))
      analytics.basis.map(_.drawdownFrequency) mustBe Some("daily")
    }
  }
}
