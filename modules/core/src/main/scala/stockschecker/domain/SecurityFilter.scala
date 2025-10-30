package stockschecker.domain

import cats.data.NonEmptyList
import org.latestbit.circe.adt.codec.*
import stockschecker.common.JsonCodecs

import scala.concurrent.duration.FiniteDuration

enum SecurityFilter derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case ExchangeIs(exchange: Exchange)
  case KindIs(kind: SecurityKind)
  case IsActive(active: Boolean)
  case UpdatedWithin(duration: FiniteDuration)
  case NotUpdatedFor(duration: FiniteDuration)
  case Composite(filters: NonEmptyList[SecurityFilter])

object SecurityFilter extends JsonCodecs {
  given JsonTaggedAdt.Config[SecurityFilter] = JsonTaggedAdt.Config.Values[SecurityFilter](
    mappings = Map(
      "exchange-is"     -> JsonTaggedAdt.tagged[SecurityFilter.ExchangeIs],
      "kind-is"         -> JsonTaggedAdt.tagged[SecurityFilter.KindIs],
      "is-active"       -> JsonTaggedAdt.tagged[SecurityFilter.IsActive],
      "updated-within"  -> JsonTaggedAdt.tagged[SecurityFilter.UpdatedWithin],
      "not-updated-for" -> JsonTaggedAdt.tagged[SecurityFilter.NotUpdatedFor],
      "composite"       -> JsonTaggedAdt.tagged[SecurityFilter.Composite]
    ),
    strict = true,
    typeFieldName = "kind"
  )
}
