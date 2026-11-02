package stockschecker.domain

import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class RankingReadinessSpec extends AnyWordSpec with Matchers:
  private val now     = Instant.parse("2026-11-02T12:00:00Z")
  private val recipe  = InvestmentRecipe()
  private val ranking = InvestmentRanking(
    InvestmentRankingId("published"),
    now,
    recipe,
    InvestmentRankingSummary(9844, 0, Map("insufficient-peer-coverage" -> 57))
  )
  private val industries = Map("software" -> 19, "medical devices" -> 19, "machinery" -> 19)
  private val sectors    = Map(Sector.Technology -> 19, Sector.Healthcare -> 19, Sector.Industrials -> 19)

  "RankingReadiness" should {
    "explain fifty-seven complete candidates that still produced no eligible stocks" in {
      val readiness = RankingReadiness.from(ranking, industries, sectors, now, recipe)
      readiness.status mustBe RankingReadinessStatus.InsufficientEligible
      readiness.rankingId mustBe ranking.id
      readiness.asOf mustBe now
      readiness.recipeVersion mustBe recipe.version
      readiness.eligibleCount mustBe 0
      readiness.requiredEligibleCount mustBe 20
      readiness.completeCandidateCount mustBe 57
      readiness.minIndustryPeers mustBe 20
      readiness.minSectorPeers mustBe 30
      readiness.completeByIndustry mustBe industries
      readiness.completeBySector mustBe sectors
    }

    "use the published eligible count without repeating peer scoring" in {
      val ready = ranking.copy(summary = ranking.summary.copy(eligibleCount = 20))
      RankingReadiness.from(ready, industries, sectors, now, recipe).status mustBe RankingReadinessStatus.Ready
    }

    "require the same UTC day even when the ranking is less than two days old" in {
      val previousDay = ranking.copy(createdAt = Instant.parse("2026-11-01T23:59:59Z"))
      RankingReadiness.from(previousDay, industries, sectors, now, recipe).status mustBe RankingReadinessStatus.StaleRanking
      val earlierToday = ranking.copy(createdAt = Instant.parse("2026-11-02T00:00:00Z"))
      RankingReadiness.from(earlierToday, industries, sectors, now, recipe).status mustBe RankingReadinessStatus.InsufficientEligible
    }

    "report a recipe mismatch before checking freshness and coverage" in {
      val previousRecipe = ranking.copy(createdAt = now.minusSeconds(86400), recipe = recipe.copy(version = "previous"))
      val readiness      = RankingReadiness.from(previousRecipe, industries, sectors, now, recipe)
      readiness.status mustBe RankingReadinessStatus.RecipeMismatch
      readiness.recipeVersion mustBe "previous"
    }

    "report the frozen recipe thresholds and count complete candidates by sector" in {
      val custom    = ranking.copy(recipe = recipe.copy(minIndustryPeers = 25, minSectorPeers = 35))
      val readiness = RankingReadiness.from(custom, Map.empty, sectors, now, recipe)
      readiness.completeCandidateCount mustBe 57
      readiness.minIndustryPeers mustBe 25
      readiness.minSectorPeers mustBe 35
    }

    "encode stable status values and sector counts" in {
      val readiness = RankingReadiness.from(ranking, industries, sectors, now, recipe)
      readiness.asJson.hcursor.get[String]("status") mustBe Right("insufficientEligible")
      readiness.asJson.as[RankingReadiness] mustBe Right(readiness)
      RankingReadinessStatus.values.map(_.asJson.asString.get).toList mustBe
        List("ready", "insufficientEligible", "staleRanking", "recipeMismatch")
    }
  }
