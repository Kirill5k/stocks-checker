package stockschecker.domain

import io.circe.Codec
import stockschecker.common.types.StringType
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
) derives Codec.AsObject

final case class InvestmentRankingSummary(
    universeCount: Int,
    eligibleCount: Int,
    excludedByReason: Map[String, Int]
) derives Codec.AsObject

object InvestmentRankingSummary:
  def from(run: InvestmentRankingRun): InvestmentRankingSummary =
    InvestmentRankingSummary(
      run.inputs.size,
      run.scorecards.count(_.eligible),
      run.scorecards.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _)
    )

final case class InvestmentRankingHeader(
    id: InvestmentRankingId,
    createdAt: Instant,
    recipe: InvestmentRecipe,
    summary: InvestmentRankingSummary
)

object InvestmentRankingHeader:
  def from(run: InvestmentRankingRun): InvestmentRankingHeader =
    InvestmentRankingHeader(run.id, run.createdAt, run.recipe, InvestmentRankingSummary.from(run))

final case class InvestmentRankingMetadata(
    id: InvestmentRankingId,
    createdAt: Instant,
    recipe: InvestmentRecipe,
    summary: Option[InvestmentRankingSummary]
):
  def resolved(summary: InvestmentRankingSummary): InvestmentRankingHeader =
    InvestmentRankingHeader(id, createdAt, recipe, summary)

final case class InvestmentRankingInputs(inputs: List[InvestmentCandidate], scorecards: List[InvestmentScorecard])

final case class InvestmentRankingRun(
    id: InvestmentRankingId,
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
    rankingId: InvestmentRankingId = InvestmentRankingId("")
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

object InvestmentScreenSummary:
  def from(run: InvestmentScreenRun): InvestmentScreenSummary =
    InvestmentScreenSummary(
      run.id,
      run.createdAt,
      run.recipe.version,
      run.inputs.size,
      run.scorecards.count(_.eligible),
      run.cohorts.find(_.name == "investment").fold(0)(_.tickers.size),
      run.scorecards.flatMap(_.exclusions).groupMapReduce(identity)(_ => 1)(_ + _)
    )

final case class InvestmentScreenHeader(
    id: InvestmentScreenId,
    createdAt: Instant,
    rankingId: InvestmentRankingId,
    request: InvestmentScreenRequest,
    cohorts: List[InvestmentCohort],
    summary: InvestmentScreenSummary
)

object InvestmentScreenHeader:
  def from(run: InvestmentScreenRun): InvestmentScreenHeader =
    InvestmentScreenHeader(run.id, run.createdAt, run.rankingId, run.request, run.cohorts, InvestmentScreenSummary.from(run))

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
