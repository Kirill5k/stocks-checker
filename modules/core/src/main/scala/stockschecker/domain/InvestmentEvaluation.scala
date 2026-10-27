package stockschecker.domain

import io.circe.Codec
import java.time.{Instant, LocalDate}

final case class EvaluationClose(
    date: LocalDate,
    totalReturnClose: BigDecimal
) derives Codec.AsObject

final case class EvaluationPriceSeries(
    ticker: Ticker,
    closes: List[EvaluationClose],
    source: String,
    fetchedAt: Instant,
    audited: Boolean,
    adjustment: String,
    completeRange: DateRange,
    issues: List[String] = Nil,
    snapshotIds: List[String] = Nil
) derives Codec.AsObject

final case class EvaluationCapturePlan(
    screenId: InvestmentScreenId,
    month: String,
    through: LocalDate,
    createdAt: Instant
) derives Codec.AsObject

final case class EvaluationPriceEvidence(
    ticker: Ticker,
    source: String,
    fetchedAt: Instant,
    adjustment: String,
    audited: Boolean,
    snapshotIds: List[String]
) derives Codec.AsObject

final case class InvestmentConcentration(
    constituentCount: Int,
    maxPositionWeightPercent: BigDecimal,
    sectorWeightsPercent: Map[String, BigDecimal],
    industryWeightsPercent: Map[String, BigDecimal],
    positionHerfindahl: BigDecimal
) derives Codec.AsObject

final case class InvestmentCohort(
    name: String,
    tickers: List[Ticker],
    concentration: Option[InvestmentConcentration] = None
) derives Codec.AsObject

final case class EvaluationCostScenario(
    roundTripBasisPoints: Int,
    totalReturnPercent: BigDecimal,
    annualizedReturnPercent: BigDecimal
) derives Codec.AsObject

final case class CohortEvaluation(
    name: String,
    status: String,
    constituents: List[Ticker],
    missing: List[Ticker],
    issues: List[String],
    totalReturnPercent: Option[BigDecimal],
    annualizedReturnPercent: Option[BigDecimal],
    maxDrawdownPercent: Option[BigDecimal],
    excessReturnPercent: Option[BigDecimal],
    costs: List[EvaluationCostScenario],
    observedThrough: Option[LocalDate] = None,
    observedReturnPercent: Option[BigDecimal] = None,
    concentration: Option[InvestmentConcentration] = None
) derives Codec.AsObject

final case class HorizonEvaluation(
    years: Int,
    status: String,
    entryDate: Option[LocalDate],
    targetDate: LocalDate,
    exitDate: Option[LocalDate],
    cohorts: List[CohortEvaluation],
    issues: List[String]
) derives Codec.AsObject

final case class InvestmentEvaluation(
    screenId: InvestmentScreenId,
    evaluatedAt: Instant,
    methodologyVersion: String,
    methodology: String,
    horizons: List[HorizonEvaluation],
    sources: List[String],
    priceEvidence: List[EvaluationPriceEvidence] = Nil,
    pendingDataJobs: List[JobId] = Nil,
    dataThrough: Option[LocalDate] = None,
    captureMonth: Option[String] = None,
    // A settled monthly capture stops fetching; it may still contain unaudited or missing data. None identifies legacy reports.
    settled: Option[Boolean] = None
) derives Codec.AsObject {
  def isComplete: Boolean =
    pendingDataJobs.isEmpty && dataThrough.nonEmpty && horizons.nonEmpty &&
      horizons.forall(h =>
        h.entryDate.nonEmpty && h.cohorts.nonEmpty && h.cohorts.forall(c => c.status != "incomplete" && c.missing.isEmpty)
      )

  // Legacy reports without a lifecycle marker are final only when all required data is auditable.
  def isSettled: Boolean = settled.getOrElse(isComplete)
}
