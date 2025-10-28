package stockschecker.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.alphavantage.AlphaVantageClient
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.common.config.ClientsConfig
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.SttpBackend

trait Clients[F[_]]:
  def marketData: MarketDataClient[F]

object Clients:
  def make[F[_]: Async](config: ClientsConfig, backend: SttpBackend[F, Fs2Streams[F]]): F[Clients[F]] =
    for
      finnhubClient      <- FinnhubClient.make[F](config.finnhub, backend)
      alphaVantageClient <- AlphaVantageClient.make(config.alphaVantage, backend)
      marketDataClient   <- MarketDataClient.make[F](finnhubClient, alphaVantageClient)
    yield new Clients[F]:
      def marketData: MarketDataClient[F] = marketDataClient
