package stockschecker.common

import cats.effect.Async
import pureconfig.*
import kirill5k.common.http4s.*

import scala.concurrent.duration.FiniteDuration
import stockschecker.clients.ProviderLimits

object config {
  final case class ApiConfig(
      key: String
  ) derives ConfigReader

  final case class ServerConfig(
      host: String,
      port: Int
  ) derives ConfigReader

  object ServerConfig:
    given Conversion[ServerConfig, Server.Config] =
      (sc: ServerConfig) => Server.Config(sc.host, sc.port)

  final case class FinnhubClientConfig(
      baseUri: String,
      apiKey: String,
      limits: ProviderLimits = ProviderLimits()
  ) derives ConfigReader

  final case class AlphaVantageClientConfig(
      baseUri: String,
      apiKey: String
  ) derives ConfigReader

  final case class TwelveDataConfig(
      baseUri: String,
      apiKey: String,
      limits: ProviderLimits = ProviderLimits(8, 800, 1)
  ) derives ConfigReader

  final case class ClientsConfig(
      finnhub: FinnhubClientConfig,
      alphaVantage: AlphaVantageClientConfig,
      twelveData: TwelveDataConfig
  ) derives ConfigReader

  final case class MongoConfig(
      user: String,
      password: String,
      host: String,
      dbName: String,
      connectTimeout: FiniteDuration,
      readTimeout: FiniteDuration,
      serverSelectionTimeout: FiniteDuration
  ) derives ConfigReader

  final case class ActionExecutorConfig(
      concurrency: Int
  ) derives ConfigReader

  final case class InvestmentConfig(
      enabled: Boolean = false,
      coverageAudited: Boolean = false,
      totalReturnAudited: Boolean = false,
      priceFreshnessDays: Int = 7,
      snapshotFreshnessDays: Int = 14,
      quarterlyFreshnessDays: Int = 180,
      annualFreshnessDays: Int = 550
  ) derives ConfigReader

  final case class AppConfig(
      api: ApiConfig,
      server: ServerConfig,
      clients: ClientsConfig,
      mongo: MongoConfig,
      actionExecutor: ActionExecutorConfig,
      investment: InvestmentConfig = InvestmentConfig()
  ) derives ConfigReader

  object AppConfig:
    def loadDefault[F[_]](using F: Async[F]): F[AppConfig] =
      F.blocking(ConfigSource.default.loadOrThrow[AppConfig])
}
