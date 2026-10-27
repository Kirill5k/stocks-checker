package stockschecker.clients

import cats.effect.Async
import kirill5k.common.cats.Clock
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient
import stockschecker.common.config.ClientsConfig
import stockschecker.repositories.ProviderBudgetRepository
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend

trait Clients[F[_]]:
  def marketData: MarketDataClient[F]

object Clients:
  def make[F[_]: {Async, Clock}](
      config: ClientsConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      budgets: ProviderBudgetRepository[F]
  ): F[Clients[F]] =
    for
      access           <- ProviderAccess.make(budgets)
      finnhubClient    <- FinnhubClient.make[F](config.finnhub, backend, access)
      twelveDataClient <- TwelveDataClient.make(config.twelveData, backend, access)
      marketDataClient <- MarketDataClient.make[F](finnhubClient, twelveDataClient)
    yield new Clients[F]:
      def marketData: MarketDataClient[F] = marketDataClient
