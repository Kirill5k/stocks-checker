package stockschecker.common

import cats.effect.Async
import pureconfig.*
import pureconfig.error.UserValidationFailed
import stockschecker.domain.InvestmentRecipe
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
      limits: ProviderLimits
  ) derives ConfigReader

  final case class AlphaVantageClientConfig(
      baseUri: String,
      apiKey: String
  ) derives ConfigReader

  final case class TwelveDataConfig(
      baseUri: String,
      apiKey: String,
      limits: ProviderLimits
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
      priceFreshnessDays: Int,
      snapshotFreshnessDays: Int,
      quarterlyFreshnessDays: Int,
      annualFreshnessDays: Int
  ):
    def recipe: InvestmentRecipe = InvestmentRecipe(
      maxSnapshotAgeDays = snapshotFreshnessDays,
      maxAnnualAgeDays = annualFreshnessDays,
      maxPriceAgeDays = priceFreshnessDays,
      maxQuarterlyAgeDays = quarterlyFreshnessDays
    )

  object InvestmentConfig:
    given ConfigReader[InvestmentConfig] = ConfigReader.derived[InvestmentConfig].emap { config =>
      config.recipe.validate.left.map(UserValidationFailed(_)).map(_ => config)
    }

  final case class AppConfig(
      api: ApiConfig,
      server: ServerConfig,
      clients: ClientsConfig,
      mongo: MongoConfig,
      actionExecutor: ActionExecutorConfig,
      investment: InvestmentConfig
  ) derives ConfigReader

  object AppConfig:
    def loadDefault[F[_]](using F: Async[F]): F[AppConfig] =
      F.blocking(ConfigSource.default.loadOrThrow[AppConfig])
}
