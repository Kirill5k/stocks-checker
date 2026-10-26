package stockschecker.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient
import stockschecker.common.config.ClientsConfig
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository}

trait Clients[F[_]]:
  def marketData: MarketDataClient[F]

object Clients:
  def make[F[_]: Async](
      config: ClientsConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      quality: DataQualityRepository[F],
      providerBudgets: ProviderBudgetRepository[F]
  ): F[Clients[F]] =
    for
      finnhubClient    <- FinnhubClient.make[F](config.finnhub, backend)
      twelveDataClient <- TwelveDataClient.make(config.twelveData, backend)
      marketDataClient <- MarketDataClient
        .make[F](finnhubClient, twelveDataClient, quality, config.finnhub.limits, config.twelveData.limits, providerBudgets)
    yield new Clients[F]:
      def marketData: MarketDataClient[F] = marketDataClient
