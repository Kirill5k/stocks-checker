package stockschecker.domain

import io.circe.Codec

import java.time.{Instant, LocalDate}

/** A canonical observation dated by fiscal period; the source does not supply publication timestamps. */
final case class FinancialObservation(
    metric: FinancialMetric,
    period: LocalDate,
    value: BigDecimal
) derives Codec.AsObject:
  def unit: MetricUnit = metric.unit

enum FinancialSnapshotIssue derives Codec.AsObject:
  case InvalidSeries(frequency: String, metric: FinancialMetric)
  case InvalidObservation(frequency: String, metric: FinancialMetric)
  case DuplicatePeriod(frequency: String, metric: FinancialMetric)
  case SymbolMismatch

/** Immutable canonical fetch vintage, with source provenance for mapping audits. */
final case class FinancialSnapshot(
    id: String,
    ticker: Ticker,
    provider: Provider,
    observedAt: Instant,
    metrics: Map[FinancialMetric, BigDecimal],
    annual: List[FinancialObservation],
    quarterly: List[FinancialObservation],
    payloadHash: String,
    mappingVersion: String,
    issues: List[FinancialSnapshotIssue]
) derives Codec.AsObject

final case class FinancialData(
    metrics: FinancialMetrics,
    snapshot: FinancialSnapshot
) derives Codec.AsObject
