package stockschecker.domain

import io.circe.Codec
import java.time.{Instant, LocalDate, ZoneOffset}

/** A provider vintage kept in a bounded cache; its ID can remain as provenance after raw candles have been pruned. */
final case class PriceHistorySnapshot(
    id: String,
    ticker: Ticker,
    candles: List[PriceCandle],
    provider: Provider,
    currency: String,
    adjustment: PriceAdjustment,
    fetchedAt: Instant,
    requestedRange: DateRange
) derives Codec.AsObject:
  def coversCompletedSessions(range: DateRange): Boolean =
    !requestedRange.from.isAfter(range.from) && !requestedRange.through.isBefore(range.through) &&
      !fetchedAt.isBefore(PriceHistorySnapshot.completedSessionCutoff(range.through))

object PriceHistorySnapshot:
  /** Histories captured before the next UTC midnight may contain an unfinished final session. */
  def completedSessionCutoff(through: LocalDate): Instant =
    through.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
