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

final case class InvestmentScreenSettings(enabled: Boolean = false, coverageAudited: Boolean = false)

final case class InvestmentRankingRun(
    id: String,
    createdAt: Instant,
    recipe: InvestmentRecipe,
    inputs: List[InvestmentCandidate],
    scorecards: List[InvestmentScorecard]
) derives Codec.AsObject

/** Inputs and scorecards always contain the entire scoring universe, before display filters or limits. */
final case class InvestmentScreenRun(
    id: String,
    createdAt: Instant,
    recipe: InvestmentRecipe,
    request: InvestmentScreenRequest,
    inputs: List[InvestmentCandidate],
    scorecards: List[InvestmentScorecard],
    cohorts: List[InvestmentCohort],
    rankingId: String = ""
) derives Codec.AsObject

final case class InvestmentScreenSummary(
    id: String,
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
