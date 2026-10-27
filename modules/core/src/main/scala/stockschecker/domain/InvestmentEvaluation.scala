package stockschecker.domain

import io.circe.Codec
import java.time.{Instant, LocalDate, YearMonth}

final case class EvaluationClose(
    date: LocalDate,
    totalReturnClose: BigDecimal
) derives Codec.AsObject

final case class EvaluationPriceSeries(
    ticker: Ticker,
    closes: List[EvaluationClose],
    source: Provider,
    fetchedAt: Instant,
    audited: Boolean,
    adjustment: PriceAdjustment,
    completeRange: DateRange,
    issues: List[String] = Nil,
    snapshotIds: List[String] = Nil
) derives Codec.AsObject

object EvaluationPriceSeries:
  private val ProviderCandleLimit = 5000

  def from(snapshot: PriceHistorySnapshot, range: DateRange): EvaluationPriceSeries =
    val issues = List(
      Option.when(snapshot.currency != "USD")("unsupported evaluation currency"),
      Option.when(snapshot.adjustment != PriceAdjustment.SplitsAndDividends)("total-return adjustments unavailable"),
      Option.when(snapshot.candles.size >= ProviderCandleLimit)("provider response may be truncated")
    ).flatten
    EvaluationPriceSeries(
      snapshot.ticker,
      snapshot.candles.filter(c => range.contains(c.date)).map(c => EvaluationClose(c.date, c.close)),
      snapshot.provider,
      snapshot.fetchedAt,
      issues.isEmpty,
      snapshot.adjustment,
      snapshot.requestedRange,
      issues,
      snapshotIds = List(snapshot.id)
    )

final case class EvaluationPriceEvidence(
    ticker: Ticker,
    source: Provider,
    fetchedAt: Instant,
    adjustment: PriceAdjustment,
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
    name: CohortName,
    tickers: List[Ticker],
    concentration: Option[InvestmentConcentration] = None
) derives Codec.AsObject

final case class EvaluationCostScenario(
    roundTripBasisPoints: Int,
    totalReturnPercent: BigDecimal,
    annualizedReturnPercent: BigDecimal
) derives Codec.AsObject

final case class CohortEvaluation(
    name: CohortName,
    status: EvaluationStatus,
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
    status: EvaluationStatus,
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
    sources: List[Provider],
    priceEvidence: List[EvaluationPriceEvidence] = Nil,
    pendingDataJobs: List[JobId] = Nil,
    dataThrough: LocalDate,
    captureMonth: YearMonth,
    // A settled monthly capture stops fetching; it may still contain unaudited or missing data.
    settled: Boolean = false
) derives Codec.AsObject {
  def isComplete: Boolean =
    pendingDataJobs.isEmpty && horizons.nonEmpty &&
      horizons.forall(h =>
        h.entryDate.nonEmpty && h.cohorts.nonEmpty && h.cohorts.forall(c => c.status != EvaluationStatus.Incomplete && c.missing.isEmpty)
      )

  def isFinished: Boolean =
    settled && horizons.nonEmpty &&
      horizons.forall { horizon =>
        val lastPossibleExit = horizon.targetDate.plusDays(if horizon.entryDate.isEmpty then 14 else 7)
        horizon.exitDate.exists(date => !date.isAfter(dataThrough)) || !lastPossibleExit.isAfter(dataThrough)
      }
}

object InvestmentEvaluation:
  // Shift the next-day entry fallback before adding years, including leap-day crossings.
  def finalCaptureThrough(formed: LocalDate): LocalDate = formed.plusDays(1).plusYears(5).plusDays(14)

  /** Capture completed calendar months; stop requesting data once the last horizon can be resolved. */
  def captureThrough(formed: LocalDate, month: YearMonth): LocalDate =
    List(month.atDay(1).minusDays(1), finalCaptureThrough(formed)).min
