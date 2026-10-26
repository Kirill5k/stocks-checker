package stockschecker.domain

import io.circe.Codec
import java.time.Instant

final case class InvestmentScreenRequest(
    limit: Option[Int] = None,
    exchange: Option[Exchange] = None,
    country: Option[String] = None,
    minScore: Option[BigDecimal] = None,
    minQuality: Option[BigDecimal] = None,
    minValue: Option[BigDecimal] = None,
    minGrowth: Option[BigDecimal] = None,
    minResilience: Option[BigDecimal] = None,
    recipeVersion: Option[String] = None
) derives Codec.AsObject

final case class InvestmentRankingSummary(
    universeCount: Int,
    eligibleCount: Int,
    excludedByReason: Map[String, Int]
) derives Codec.AsObject

final case class InvestmentRankingRun(
    id: String,
    createdAt: Instant,
    recipe: InvestmentRecipe,
    inputs: List[InvestmentCandidate],
    scorecards: List[InvestmentScorecard]
) derives Codec.AsObject

/** The entire scoring universe is retained before display filters or limits. Persisted inputs contain stock metadata and compact financial
  * diagnostics; scorecards preserve the observations actually used by the recipe.
  */
final case class InvestmentScreenRun(
    id: InvestmentScreenId,
    createdAt: Instant,
    recipe: InvestmentRecipe,
    request: InvestmentScreenRequest,
    inputs: List[InvestmentCandidate],
    scorecards: List[InvestmentScorecard],
    cohorts: List[InvestmentCohort],
    rankingId: String = ""
) derives Codec.AsObject

final case class InvestmentScreenSummary(
    id: InvestmentScreenId,
    createdAt: Instant,
    modelVersion: String,
    universeCount: Int,
    eligibleCount: Int,
    selectedCount: Int,
    excludedByReason: Map[String, Int]
) derives Codec.AsObject

final case class InvestmentNearMiss(scorecard: InvestmentScorecard, failedRule: String) derives Codec.AsObject
final case class InvestmentScreenResult(
    summary: InvestmentScreenSummary,
    request: InvestmentScreenRequest,
    candidates: List[InvestmentScorecard],
    nearMisses: List[InvestmentNearMiss],
    exclusions: List[InvestmentScorecard],
    cohorts: List[InvestmentCohort],
    rankingId: String
) derives Codec.AsObject
