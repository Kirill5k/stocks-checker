package stockschecker.common

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import pureconfig.ConfigSource
import stockschecker.common.config.InvestmentConfig

class InvestmentConfigSpec extends AnyWordSpec with Matchers {
  private def source(priceDays: Int = 7, snapshotDays: Int = 30, quarterlyDays: Int = 150, annualDays: Int = 450): ConfigSource =
    ConfigSource.string(s"""
      price-freshness-days = $priceDays
      snapshot-freshness-days = $snapshotDays
      quarterly-freshness-days = $quarterlyDays
      annual-freshness-days = $annualDays
    """)

  "InvestmentConfig" should {
    "load freshness limits into the investment recipe" in {
      val recipe = source().loadOrThrow[InvestmentConfig].recipe
      recipe.maxPriceAgeDays mustBe 7
      recipe.maxSnapshotAgeDays mustBe 30
      recipe.maxQuarterlyAgeDays mustBe 150
      recipe.maxAnnualAgeDays mustBe 450
    }

    "reject invalid freshness limits while reading configuration" in
      List(source(priceDays = 0), source(snapshotDays = -1), source(quarterlyDays = 0), source(annualDays = -1)).foreach { config =>
        val errors = config.load[InvestmentConfig].left.toOption.get
        errors.prettyPrint() must include("Freshness limits must be positive")
      }
  }
}
