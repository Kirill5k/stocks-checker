package stockschecker.domain

import io.circe.Codec
import java.time.{Instant, LocalDate}

/** A provider vintage kept in a bounded cache; its ID can remain as provenance after raw candles have been pruned. */
final case class PriceHistorySnapshot(
    id: String,
    ticker: Ticker,
    candles: List[PriceCandle],
    provider: String,
    currency: String,
    adjustment: String,
    fetchedAt: Instant,
    requestedFrom: LocalDate,
    requestedThrough: LocalDate
) derives Codec.AsObject
