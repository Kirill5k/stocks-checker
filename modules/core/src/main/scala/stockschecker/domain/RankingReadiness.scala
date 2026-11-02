package stockschecker.domain

import io.circe.Codec
import stockschecker.common.types.EnumType

import java.time.{Instant, ZoneOffset}

enum RankingReadinessStatus(val value: String):
  case Ready                extends RankingReadinessStatus("ready")
  case InsufficientEligible extends RankingReadinessStatus("insufficientEligible")
  case StaleRanking         extends RankingReadinessStatus("staleRanking")
  case RecipeMismatch       extends RankingReadinessStatus("recipeMismatch")

object RankingReadinessStatus extends EnumType[RankingReadinessStatus](() => RankingReadinessStatus.values, _.value)

final case class RankingReadiness(
    rankingId: InvestmentRankingId,
    asOf: Instant,
    recipeVersion: String,
    status: RankingReadinessStatus,
    eligibleCount: Int,
    requiredEligibleCount: Int,
    completeCandidateCount: Int,
    minIndustryPeers: Int,
    minSectorPeers: Int,
    completeByIndustry: Map[String, Int],
    completeBySector: Map[Sector, Int]
) derives Codec.AsObject

object RankingReadiness:
  def from(
      ranking: InvestmentRanking,
      completeByIndustry: Map[String, Int],
      completeBySector: Map[Sector, Int],
      at: Instant,
      expectedRecipe: InvestmentRecipe
  ): RankingReadiness =
    val status =
      if ranking.recipe.version != expectedRecipe.version then RankingReadinessStatus.RecipeMismatch
      else if ranking.createdAt.atZone(ZoneOffset.UTC).toLocalDate != at.atZone(ZoneOffset.UTC).toLocalDate then
        RankingReadinessStatus.StaleRanking
      else if ranking.summary.eligibleCount < InvestmentRecipe.MinimumEligibleStocks then RankingReadinessStatus.InsufficientEligible
      else RankingReadinessStatus.Ready
    RankingReadiness(
      ranking.id,
      ranking.createdAt,
      ranking.recipe.version,
      status,
      ranking.summary.eligibleCount,
      InvestmentRecipe.MinimumEligibleStocks,
      completeBySector.values.sum,
      ranking.recipe.minIndustryPeers,
      ranking.recipe.minSectorPeers,
      completeByIndustry,
      completeBySector
    )
