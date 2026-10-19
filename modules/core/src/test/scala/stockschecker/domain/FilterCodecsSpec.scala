package stockschecker.domain

import cats.data.NonEmptyList
import io.circe.parser.{decode, parse}
import io.circe.syntax.*
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

import java.time.LocalDate
import scala.concurrent.duration.*

class FilterCodecsSpec extends AnyWordSpec with Matchers with EitherValues with TableDrivenPropertyChecks {

  "CompanyProfileFilter codec" should {
    "preserve the legacy JSON for every variant and recursive composites" in {
      val testCases = Table[CompanyProfileFilter, String](
        ("filter", "json"),
        (CompanyProfileFilter.IsActive(true), """{"kind":"is-active","active":true}"""),
        (CompanyProfileFilter.MarketCapAbove(10000000000L), """{"kind":"market-cap-above","min":10000000000}"""),
        (CompanyProfileFilter.MarketCapBelow(20000000000L), """{"kind":"market-cap-below","max":20000000000}"""),
        (CompanyProfileFilter.CountryIs("US"), """{"kind":"country-is","countryCode":"US"}"""),
        (CompanyProfileFilter.IpoDateAfter(LocalDate.of(2020, 1, 2)), """{"kind":"ipo-date-after","date":"2020-01-02"}"""),
        (CompanyProfileFilter.IpoDateBefore(LocalDate.of(2024, 2, 29)), """{"kind":"ipo-date-before","date":"2024-02-29"}"""),
        (CompanyProfileFilter.UpdatedWithin(1.minute), """{"kind":"updated-within","duration":"1minute"}"""),
        (CompanyProfileFilter.NotUpdatedFor(2.hours), """{"kind":"not-updated-for","duration":"2hours"}"""),
        (CompanyProfileFilter.PriceAnalyticsNotUpdatedFor(3.days), """{"kind":"price-analytics-not-updated-for","duration":"3days"}"""),
        (
          CompanyProfileFilter.FinancialMetricsNotUpdatedFor(500.millis),
          """{"kind":"financial-metrics-not-updated-for","duration":"500milliseconds"}"""
        ),
        (CompanyProfileFilter.TickerMatching("^A.*"), """{"kind":"ticker-matching","pattern":"^A.*"}"""),
        (
          CompanyProfileFilter.Composite(
            NonEmptyList.of(
              CompanyProfileFilter.IsActive(false),
              CompanyProfileFilter.Composite(NonEmptyList.one(CompanyProfileFilter.CountryIs("GB")))
            )
          ),
          """{"kind":"composite","filters":[{"kind":"is-active","active":false},{"kind":"composite","filters":[{"kind":"country-is","countryCode":"GB"}]}]}"""
        )
      )

      forAll(testCases) { (filter, json) =>
        filter.asJson mustBe parse(json).value
        decode[CompanyProfileFilter](json) mustBe Right(filter)
      }
    }

    "reject missing required fields and invalid composite contents" in {
      val testCases = Table(
        "json",
        """{"kind":"is-active"}""",
        """{"kind":"market-cap-above"}""",
        """{"kind":"market-cap-below"}""",
        """{"kind":"country-is"}""",
        """{"kind":"ipo-date-after"}""",
        """{"kind":"ipo-date-before"}""",
        """{"kind":"updated-within"}""",
        """{"kind":"not-updated-for"}""",
        """{"kind":"price-analytics-not-updated-for"}""",
        """{"kind":"financial-metrics-not-updated-for"}""",
        """{"kind":"ticker-matching"}""",
        """{"kind":"composite"}""",
        """{"kind":"composite","filters":[]}""",
        """{"kind":"composite","filters":[{"kind":"country-is"}]}"""
      )

      forAll(testCases)(json => decode[CompanyProfileFilter](json).isLeft mustBe true)
    }
  }

  "SecurityFilter codec" should {
    "preserve the legacy JSON for every variant and recursive composites" in {
      val testCases = Table[SecurityFilter, String](
        ("filter", "json"),
        (SecurityFilter.ExchangeIs(Exchange.NASDAQ), """{"kind":"exchange-is","exchange":"nasdaq"}"""),
        (SecurityFilter.ExchangeIs(Exchange.NYSE), """{"kind":"exchange-is","exchange":"nyse"}"""),
        (SecurityFilter.KindIs(SecurityKind.Stock), """{"kind":"kind-is","securityKind":"stock"}"""),
        (SecurityFilter.KindIs(SecurityKind.ETF), """{"kind":"kind-is","securityKind":"etf"}"""),
        (SecurityFilter.IsActive(false), """{"kind":"is-active","active":false}"""),
        (SecurityFilter.UpdatedWithin(1.minute), """{"kind":"updated-within","duration":"1minute"}"""),
        (SecurityFilter.NotUpdatedFor(2.hours), """{"kind":"not-updated-for","duration":"2hours"}"""),
        (SecurityFilter.CompanyProfileNotUpdatedFor(3.days), """{"kind":"company-profile-not-updated-for","duration":"3days"}"""),
        (
          SecurityFilter.Composite(
            NonEmptyList.of(
              SecurityFilter.ExchangeIs(Exchange.NYSE),
              SecurityFilter.Composite(NonEmptyList.one(SecurityFilter.KindIs(SecurityKind.REIT)))
            )
          ),
          """{"kind":"composite","filters":[{"kind":"exchange-is","exchange":"nyse"},{"kind":"composite","filters":[{"kind":"kind-is","securityKind":"reit"}]}]}"""
        )
      )

      forAll(testCases) { (filter, json) =>
        filter.asJson mustBe parse(json).value
        decode[SecurityFilter](json) mustBe Right(filter)
      }
    }

    "reject missing required fields and invalid composite contents" in {
      val testCases = Table(
        "json",
        """{"kind":"exchange-is"}""",
        """{"kind":"kind-is"}""",
        """{"kind":"is-active"}""",
        """{"kind":"updated-within"}""",
        """{"kind":"not-updated-for"}""",
        """{"kind":"company-profile-not-updated-for"}""",
        """{"kind":"composite"}""",
        """{"kind":"composite","filters":[]}""",
        """{"kind":"composite","filters":[{"kind":"kind-is"}]}"""
      )

      forAll(testCases)(json => decode[SecurityFilter](json).isLeft mustBe true)
    }
  }

  "PriceAnalyticsFilter codec" should {
    "preserve the legacy JSON for every variant and recursive composites" in {
      val testCases = Table[PriceAnalyticsFilter, String](
        ("filter", "json"),
        (PriceAnalyticsFilter.UpdatedWithin(1.minute), """{"kind":"updated-within","duration":"1minute"}"""),
        (PriceAnalyticsFilter.NotUpdatedFor(2.hours), """{"kind":"not-updated-for","duration":"2hours"}"""),
        (PriceAnalyticsFilter.PriceAbove(BigDecimal("12.34")), """{"kind":"price-above","minPrice":12.34}"""),
        (PriceAnalyticsFilter.PriceBelow(BigDecimal("56.78")), """{"kind":"price-below","maxPrice":56.78}"""),
        (
          PriceAnalyticsFilter.PerformanceAbove(TimePeriod.OneYear, BigDecimal("15.5")),
          """{"kind":"performance-above","period":"one-year","minPercentage":15.5}"""
        ),
        (
          PriceAnalyticsFilter.PerformanceBelow(TimePeriod.ThreeMonth, BigDecimal("-2.5")),
          """{"kind":"performance-below","period":"three-month","maxPercentage":-2.5}"""
        ),
        (
          PriceAnalyticsFilter.Composite(
            NonEmptyList.of(
              PriceAnalyticsFilter.PriceAbove(BigDecimal("1.25")),
              PriceAnalyticsFilter.Composite(NonEmptyList.one(PriceAnalyticsFilter.NotUpdatedFor(3.days)))
            )
          ),
          """{"kind":"composite","filters":[{"kind":"price-above","minPrice":1.25},{"kind":"composite","filters":[{"kind":"not-updated-for","duration":"3days"}]}]}"""
        )
      )

      forAll(testCases) { (filter, json) =>
        filter.asJson mustBe parse(json).value
        decode[PriceAnalyticsFilter](json) mustBe Right(filter)
      }
    }

    "reject missing required fields and invalid composite contents" in {
      val testCases = Table(
        "json",
        """{"kind":"updated-within"}""",
        """{"kind":"not-updated-for"}""",
        """{"kind":"price-above"}""",
        """{"kind":"price-below"}""",
        """{"kind":"performance-above","period":"one-year"}""",
        """{"kind":"performance-above","minPercentage":15.5}""",
        """{"kind":"performance-below","period":"three-month"}""",
        """{"kind":"performance-below","maxPercentage":-2.5}""",
        """{"kind":"composite"}""",
        """{"kind":"composite","filters":[]}""",
        """{"kind":"composite","filters":[{"kind":"price-above"}]}"""
      )

      forAll(testCases)(json => decode[PriceAnalyticsFilter](json).isLeft mustBe true)
    }
  }

  "Filter codecs" should {
    "reject unknown, missing, or invalid discriminators" in {
      val testCases = Table("json", """{"kind":"unknown"}""", "{}", """{"kind":null}""", """{"kind":42}""")

      forAll(testCases) { json =>
        decode[CompanyProfileFilter](json).isLeft mustBe true
        decode[SecurityFilter](json).isLeft mustBe true
        decode[PriceAnalyticsFilter](json).isLeft mustBe true
      }
    }

    "continue accepting extra fields" in {
      decode[CompanyProfileFilter]("""{"kind":"country-is","countryCode":"US","extra":true}""") mustBe
        Right(CompanyProfileFilter.CountryIs("US"))
      decode[SecurityFilter]("""{"kind":"kind-is","securityKind":"stock","extra":true}""") mustBe
        Right(SecurityFilter.KindIs(SecurityKind.Stock))
      decode[PriceAnalyticsFilter]("""{"kind":"price-above","minPrice":12.34,"extra":true}""") mustBe
        Right(PriceAnalyticsFilter.PriceAbove(BigDecimal("12.34")))
    }
  }
}
