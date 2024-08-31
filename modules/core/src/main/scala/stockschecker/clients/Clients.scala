package stockschecker.clients

import cats.effect.Async
import cats.syntax.functor.*
import stockschecker.common.config.ClientsConfig
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.SttpBackend

trait Clients[F[_]]:
  def financialModelingPrep: MarketDataClient[F]

object Clients:
  def make[F[_]](config: ClientsConfig, backend: SttpBackend[F, Fs2Streams[F]])(using F: Async[F]): F[Clients[F]] =
    for fmp <- FinancialModelingPrepClient.make[F](config.financialModelingPrep, backend)
    yield new Clients[F]:
      def financialModelingPrep: MarketDataClient[F] = fmp
