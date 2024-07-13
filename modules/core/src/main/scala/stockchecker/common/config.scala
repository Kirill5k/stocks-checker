package stockchecker.common

import cats.effect.Async
import pureconfig.*
import pureconfig.generic.derivation.default.*

object config {
  final case class FinancialModelingPrepConfig(
      baseUri: String,
      apiKey: String
  ) derives ConfigReader

  final case class ClientsConfig(
      financialModelingPrep: FinancialModelingPrepConfig
  ) derives ConfigReader

  final case class AppConfig(
      clients: ClientsConfig
  ) derives ConfigReader

  object AppConfig {
    def loadDefault[F[_]](using F: Async[F]): F[AppConfig] =
      F.blocking(ConfigSource.default.loadOrThrow[AppConfig])
  }
}
