package stockschecker.common

import cats.effect.Async
import pureconfig.*
import pureconfig.generic.derivation.default.*
import kirill5k.common.http4s.*

object config {
  final case class ServerConfig(
      host: String,
      port: Int
  ) derives ConfigReader

  object ServerConfig:
    given Conversion[ServerConfig, Server.Config] =
      (sc: ServerConfig) => Server.Config(sc.host, sc.port)

  final case class FinancialModelingPrepConfig(
      baseUri: String,
      apiKey: String
  ) derives ConfigReader

  final case class ClientsConfig(
      financialModelingPrep: FinancialModelingPrepConfig
  ) derives ConfigReader

  final case class MongoConfig(
      connectionUri: String,
      dbName: String
  ) derives ConfigReader

  final case class AppConfig(
      server: ServerConfig,
      clients: ClientsConfig,
      mongo: MongoConfig
  ) derives ConfigReader

  object AppConfig {
    def loadDefault[F[_]](using F: Async[F]): F[AppConfig] =
      F.blocking(ConfigSource.default.loadOrThrow[AppConfig])
  }
}
