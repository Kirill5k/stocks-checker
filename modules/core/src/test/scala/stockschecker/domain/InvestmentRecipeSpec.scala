package stockschecker.domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InvestmentRecipeSpec extends AnyWordSpec with Matchers {
  "InvestmentRecipe" should {
    "validate a recipe before it is used to rank a universe" in {
      val recipe = InvestmentRecipe()
      recipe.validate mustBe Right(())
      recipe.weights mustBe Map(
        Pillar.Quality -> BigDecimal("0.35"),
        Pillar.Value -> BigDecimal("0.30"),
        Pillar.Growth -> BigDecimal("0.20"),
        Pillar.Resilience -> BigDecimal("0.15")
      )
      List(
        recipe.copy(qualityWeight = BigDecimal("-0.1")),
        recipe.copy(growthWeight = BigDecimal("0.5"))
      ).foreach(_.validate mustBe Left("Investment weights must be nonnegative and sum to one"))
      List(recipe.copy(minIndustryPeers = 1), recipe.copy(minSectorPeers = 1))
        .foreach(_.validate mustBe Left("Peer cohorts require at least two companies"))
      List(
        recipe.copy(maxSnapshotAgeDays = 0),
        recipe.copy(maxAnnualAgeDays = -1),
        recipe.copy(maxPriceAgeDays = 0),
        recipe.copy(maxQuarterlyAgeDays = 0)
      ).foreach(_.validate mustBe Left("Freshness limits must be positive"))
    }
  }
}
