package stockchecker.clients

import cats.effect.Async
import cats.syntax.functor.*
import stockchecker.common.config.ClientsConfig
import sttp.client3.SttpBackend

trait Clients[F[_]]:
  def financialModelingPrep: FinancialModelingPrepClient[F]

object Clients:
  def make[F[_]](config: ClientsConfig, backend: SttpBackend[F, Any])(using F: Async[F]): F[Clients[F]] =
    for fmp <- FinancialModelingPrepClient.make[F](config.financialModelingPrep, backend)
    yield new Clients[F]:
      def financialModelingPrep: FinancialModelingPrepClient[F] = fmp
