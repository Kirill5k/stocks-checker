package stockschecker.domain

import io.circe.Codec
import kirill5k.common.syntax.time.*
import stockschecker.common.types.StringType
import stockschecker.domain.investment.Eligibility
import sttp.tapir.{Codec as TapirCodec, DecodeResult, Schema}

import java.time.Instant
import java.util.UUID

opaque type InvestmentScreenId = String
object InvestmentScreenId extends StringType[InvestmentScreenId] {
  def gen: InvestmentScreenId = InvestmentScreenId(UUID.randomUUID().toString)

  inline given TapirCodec.PlainCodec[InvestmentScreenId] =
    TapirCodec.string.mapDecode[InvestmentScreenId](s => DecodeResult.Value(InvestmentScreenId(s)))(_.value)
  given Schema[InvestmentScreenId] = Schema.string
}

opaque type InvestmentRankingId = String
object InvestmentRankingId extends StringType[InvestmentRankingId] {
  def gen: InvestmentRankingId = InvestmentRankingId(UUID.randomUUID().toString)

  given Schema[InvestmentRankingId] = Schema.string
}

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
) derives Codec.AsObject:
  def validate(recipe: InvestmentRecipe): Either[String, Unit] =
    val thresholds = List(minScore, minQuality, minValue, minGrowth, minResilience).flatten
    if limit.exists(n => n < InvestmentRecipe.MinimumLimit || n > InvestmentRecipe.MaximumLimit) || thresholds.exists(n => n < 0 || n > 100)
    then Left("limit must be between 1 and 100; score thresholds must be between 0 and 100")
    else if recipeVersion.exists(_ != recipe.version) then Left(s"Unknown recipeVersion; supported version: ${recipe.version}")
    else Right(())

final case class InvestmentRankingSummary(
    universeCount: Int,
    eligibleCount: Int,
    excludedByReason: Map[String, Int]
) derives Codec.AsObject

object InvestmentRankingSummary:
  def from(entries: List[InvestmentRankingEntry]): InvestmentRankingSummary =
    InvestmentRankingSummary(
      entries.size,
      entries.count(_.scorecard.eligible),
      entries.flatMap(_.scorecard.exclusions).groupMapReduce(identity)(_ => 1)(_ + _)
    )

final case class InvestmentRanking(
    id: InvestmentRankingId,
    createdAt: Instant,
    recipe: InvestmentRecipe,
    summary: InvestmentRankingSummary
):
  def requireUsable(expectedRecipe: InvestmentRecipe, now: Instant): Either[String, InvestmentRanking] =
    if recipe.version != expectedRecipe.version then
      Left("No completed ranking for this recipe is available; wait for the scheduled ranking run")
    else if createdAt.isBefore(now.minus(InvestmentRecipe.RankingMaxAge)) then Left("The latest completed ranking is stale")
    else Right(this)

/** Only the frozen values needed to select and explain screens; scoring evidence remains in the scorecard. */
final case class InvestmentScreenInput(
    exchange: Exchange,
    country: Option[String],
    industry: Option[String],
    sector: Option[Sector],
    priceScore: Option[BigDecimal],
    exclusionExplanation: Option[String]
) derives Codec.AsObject

final case class InvestmentRankingEntry(input: InvestmentScreenInput, scorecard: InvestmentScorecard)

object InvestmentRankingEntry:
  def from(candidate: InvestmentCandidate, scorecard: InvestmentScorecard): InvestmentRankingEntry =
    val stock = candidate.stock
    InvestmentRankingEntry(
      InvestmentScreenInput(
        stock.security.exchange,
        stock.profile.map(_.country),
        stock.profile.map(_.industry),
        stock.profile.flatMap(_.sector),
        stock.priceAnalytics.map(_.scores.overallScore),
        Eligibility.explainExclusion(scorecard, candidate.financialSnapshot)
      ),
      scorecard
    )

final case class InvestmentScreenSummary(
    id: InvestmentScreenId,
    createdAt: Instant,
    modelVersion: String,
    universeCount: Int,
    eligibleCount: Int,
    selectedCount: Int,
    excludedByReason: Map[String, Int]
) derives Codec.AsObject

final case class InvestmentScreen(
    id: InvestmentScreenId,
    createdAt: Instant,
    rankingId: InvestmentRankingId,
    request: InvestmentScreenRequest,
    cohorts: List[InvestmentCohort],
    summary: InvestmentScreenSummary
)

object InvestmentScreen:
  def create(
      id: InvestmentScreenId,
      createdAt: Instant,
      ranking: InvestmentRanking,
      request: InvestmentScreenRequest,
      cohorts: List[InvestmentCohort]
  ): InvestmentScreen =
    InvestmentScreen(
      id,
      createdAt,
      ranking.id,
      request,
      cohorts,
      InvestmentScreenSummary(
        id,
        createdAt,
        ranking.recipe.version,
        ranking.summary.universeCount,
        ranking.summary.eligibleCount,
        cohorts.find(_.name == CohortName.Investment).fold(0)(_.tickers.size),
        ranking.summary.excludedByReason
      )
    )

final case class InvestmentNearMiss(scorecard: InvestmentScorecard, failedRule: String) derives Codec.AsObject
final case class InvestmentScreenResult(
    summary: InvestmentScreenSummary,
    request: InvestmentScreenRequest,
    candidates: List[InvestmentScorecard],
    nearMisses: List[InvestmentNearMiss],
    exclusions: List[InvestmentScorecard],
    cohorts: List[InvestmentCohort],
    rankingId: InvestmentRankingId
) derives Codec.AsObject
