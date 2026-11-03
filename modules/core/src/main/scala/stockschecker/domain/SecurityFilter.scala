package stockschecker.domain

import cats.data.NonEmptyList
import io.circe.derivation.{Configuration, ConfiguredCodec}
import stockschecker.common.JsonCodecs

import scala.concurrent.duration.FiniteDuration

enum SecurityFilter derives ConfiguredCodec:
  case ExchangeIs(exchange: Exchange)
  case KindIs(securityKind: SecurityKind)
  case IsActive(active: Boolean)
  case UpdatedWithin(duration: FiniteDuration)
  case NotUpdatedFor(duration: FiniteDuration)
  case CompanyProfileNotUpdatedFor(duration: FiniteDuration)
  case Composite(filters: NonEmptyList[SecurityFilter])

object SecurityFilter extends JsonCodecs:
  given Configuration = Configuration.default.withKebabCaseConstructorNames.withDiscriminator("kind").withDefaults
