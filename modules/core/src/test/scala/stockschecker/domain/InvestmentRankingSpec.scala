package stockschecker.domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class InvestmentRankingSpec extends AnyWordSpec with Matchers:
  private val now     = Instant.parse("2026-10-19T12:00:00Z")
  private val recipe  = InvestmentRecipe()
  private val ranking = InvestmentRanking(InvestmentRankingId("published"), now, recipe, InvestmentRankingSummary(0, 0, Map.empty))

  "InvestmentRanking.requireUsable" should {
    "accept the current recipe through the exact two-day freshness boundary" in {
      ranking.requireUsable(recipe, now) mustBe Right(ranking)
      val boundary = ranking.copy(createdAt = now.minusSeconds(2L * 86400))
      boundary.requireUsable(recipe, now) mustBe Right(boundary)
    }

    "reject stale rankings immediately after the freshness boundary" in {
      val stale = ranking.copy(createdAt = now.minusSeconds(2L * 86400).minusNanos(1))
      stale.requireUsable(recipe, now) mustBe Left("The latest completed ranking is stale")
    }

    "reject a ranking published under another recipe version" in {
      ranking.requireUsable(recipe.copy(version = "new-version"), now) mustBe
        Left("No completed ranking for this recipe is available; wait for the scheduled ranking run")
    }
  }
