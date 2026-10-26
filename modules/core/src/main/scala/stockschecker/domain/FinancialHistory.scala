package stockschecker.domain

import io.circe.Codec

import java.time.{Instant, LocalDate}

/** A provider observation dated by fiscal period; the source does not supply publication timestamps. */
final case class FinancialObservation(
    metric: String,
    period: LocalDate,
    value: BigDecimal,
    unit: String
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
    validationErrors: List[String]
) derives Codec.AsObject

final case class FinancialData(metrics: FinancialMetrics, snapshot: FinancialSnapshot) derives Codec.AsObject
