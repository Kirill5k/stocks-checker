package stockschecker.domain

import io.circe.Codec
import java.time.{Instant, LocalDate}
import scala.concurrent.duration.*

final case class InvestmentCandidate(stock: Stock, financialSnapshot: Option[FinancialSnapshot]) derives Codec.AsObject

final case class InvestmentEvidence(
    provider: Provider,
    observedAt: Instant,
    mappingVersion: String,
    payloadHash: String,
    annualPeriods: Map[FinancialMetric, List[LocalDate]],
    financialObservations: List[FinancialObservation],
    latestFreeCashFlowPerShareTtm: Option[FinancialObservation],
    latestPrice: Option[LatestPrice],
    quoteCurrency: Option[String]
) derives Codec.AsObject

/** Initial research hypotheses, not fitted expected-return estimates. */
final case class InvestmentRecipe(
    version: String = "quality-fair-value-v1",
    qualityWeight: BigDecimal = BigDecimal("0.35"),
    valueWeight: BigDecimal = BigDecimal("0.30"),
    growthWeight: BigDecimal = BigDecimal("0.20"),
    resilienceWeight: BigDecimal = BigDecimal("0.15"),
    minIndustryPeers: Int = 20,
    minSectorPeers: Int = 30,
    maxSnapshotAgeDays: Int = 14,
    maxAnnualAgeDays: Int = 550,
    maxPriceAgeDays: Int = 7,
    maxQuarterlyAgeDays: Int = 180
) derives Codec.AsObject:
  def weights: Map[Pillar, BigDecimal] = Map(
    Pillar.Quality    -> qualityWeight,
    Pillar.Value      -> valueWeight,
    Pillar.Growth     -> growthWeight,
    Pillar.Resilience -> resilienceWeight
  )

  def validate: Either[String, Unit] =
    if !weights.values.forall(_ >= 0) || weights.values.sum != 1 then Left("Investment weights must be nonnegative and sum to one")
    else if minIndustryPeers < 2 || minSectorPeers < 2 then Left("Peer cohorts require at least two companies")
    else if List(maxSnapshotAgeDays, maxAnnualAgeDays, maxPriceAgeDays, maxQuarterlyAgeDays).exists(_ <= 0) then
      Left("Freshness limits must be positive")
    else Right(())

object InvestmentRecipe:
  val RankingMaxAge = 2.days
  // Allow an in-flight screen creation to pin a ranking accepted just before the freshness deadline.
  val RankingRetention      = RankingMaxAge + 1.day
  val MinimumEligibleStocks = 20
  val CohortSize            = 20
  val NearMissLimit         = 10
  val MinimumLimit          = 1
  val MaximumLimit          = 100

final case class InvestmentScorecard(
    ticker: Ticker,
    eligible: Boolean,
    score: Option[BigDecimal],
    pillars: Map[Pillar, BigDecimal],
    features: Map[ScoringFeature, BigDecimal],
    exclusions: List[String],
    warnings: List[String],
    peerGroup: Option[String],
    peerCount: Int,
    modelVersion: String,
    featureUnits: Map[ScoringFeature, String] = Map.empty,
    evidence: Option[InvestmentEvidence] = None,
    strengths: List[String] = Nil,
    weaknesses: List[String] = Nil,
    contributions: Map[Pillar, BigDecimal] = Map.empty
) derives Codec.AsObject
