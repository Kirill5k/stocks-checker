package stockschecker.domain

import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.prop.TableDrivenPropertyChecks

class TimePeriodSpec extends AnyWordSpec with Matchers with EitherValues with TableDrivenPropertyChecks {

  "TimePeriod" should {
    "decode and encode into json" in {
      val testCases = Table(
        ("period", "json"),
        (TimePeriod.OneMonth, "one-month"),
        (TimePeriod.ThreeMonth, "three-month"),
        (TimePeriod.SixMonth, "six-month"),
        (TimePeriod.OneYear, "one-year"),
        (TimePeriod.ThreeYear, "three-year"),
        (TimePeriod.FiveYear, "five-year"),
        (TimePeriod.TenYear, "ten-year"),
        (TimePeriod.Max, "max")
      )

      forAll(testCases) { (period, json) =>
        period.asJson.noSpaces mustBe s""""$json""""
        decode[TimePeriod](s""""$json"""") mustBe Right(period)
      }
    }
  }
}
