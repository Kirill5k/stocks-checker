package stockschecker.domain

import io.circe.Codec

import java.time.{Instant, LocalDate}

/** A provider observation. Fiscal period is deliberately separate from publication time. */
final case class FinancialObservation(
    metric: String,
    period: LocalDate,
    value: BigDecimal,
    unit: String,
    publishedAt: Option[Instant] = None
) derives Codec.AsObject

/** Immutable fetch vintage, including the original response for subsequent mapping audits. */
final case class FinancialSnapshot(
    id: String,
    ticker: Ticker,
    provider: String,
    observedAt: Instant,
    metrics: Map[String, BigDecimal],
    annual: List[FinancialObservation],
    quarterly: List[FinancialObservation],
    rawPayload: String,
    payloadHash: String,
    mappingVersion: String,
    validationErrors: List[String],
    schemaVersion: Int = 1,
    publishedAt: Option[Instant] = None,
    reportingCurrency: Option[String] = None
) derives Codec.AsObject

final case class FinancialData(metrics: FinancialMetrics, snapshot: FinancialSnapshot) derives Codec.AsObject
