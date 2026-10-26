package stockschecker.domain

import io.circe.Codec
import java.time.{Instant, LocalDate}

final case class InvestmentCandidate(stock: Stock, financialSnapshot: Option[FinancialSnapshot]) derives Codec.AsObject

final case class InvestmentEvidence(
    provider: String,
    observedAt: Instant,
    mappingVersion: String,
    payloadHash: String,
    annualPeriods: Map[String, List[LocalDate]],
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
) derives Codec.AsObject

final case class InvestmentScorecard(
    ticker: Ticker,
    eligible: Boolean,
    score: Option[BigDecimal],
    pillars: Map[String, BigDecimal],
    features: Map[String, BigDecimal],
    exclusions: List[String],
    warnings: List[String],
    peerGroup: Option[String],
    peerCount: Int,
    financialSnapshotId: Option[String],
    modelVersion: String,
    featureUnits: Map[String, String] = Map.empty,
    evidence: Option[InvestmentEvidence] = None,
    strengths: List[String] = Nil,
    weaknesses: List[String] = Nil,
    contributions: Map[String, BigDecimal] = Map.empty
) derives Codec.AsObject
