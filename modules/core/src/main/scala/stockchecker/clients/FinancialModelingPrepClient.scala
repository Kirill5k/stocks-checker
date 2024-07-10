package stockchecker.clients

import cats.effect.Async
import stockchecker.common.config.FinancialModelingPrepConfig
import stockchecker.domain.Ticker
import sttp.client3.SttpBackend

trait FinancialModelingPrepClient[F[_]]:
  def allTradedStocks: F[List[Ticker]]

final private class LiveFinancialModelingPrepClient[F[_]](
    private val config: FinancialModelingPrepConfig,
    private val backend: SttpBackend[F, Any]
)(using
    F: Async[F]
) extends FinancialModelingPrepClient[F] {

  override def allTradedStocks: F[List[Ticker]] = ???

}

object FinancialModelingPrepClient:
  def make[F[_]](config: FinancialModelingPrepConfig, backend: SttpBackend[F, Any])(using F: Async[F]): F[FinancialModelingPrepClient[F]] =
    F.pure(LiveFinancialModelingPrepClient[F](config, backend))
