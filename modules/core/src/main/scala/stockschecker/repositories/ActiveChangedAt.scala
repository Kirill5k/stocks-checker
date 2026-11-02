package stockschecker.repositories

import mongo4cats.operations.Filter

import java.time.Instant
import java.time.format.DateTimeFormatterBuilder

private[repositories] object ActiveChangedAt:
  // BSON dates discard sub-millisecond precision. Fixed-width UTC strings retain request ordering.
  private val format = new DateTimeFormatterBuilder().appendInstant(9).toFormatter

  def encode(requestedAt: Instant): String = format.format(requestedAt)

  def accepts(field: String, requestedAt: Instant): Filter =
    Filter.isNull(field) || Filter.lte(field, encode(requestedAt))
