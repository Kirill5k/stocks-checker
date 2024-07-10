package stockchecker.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import io.circe.Codec
import stockchecker.common.config.FinancialModelingPrepConfig
import stockchecker.domain.{Symbol, Ticker}
import sttp.client3.*
import sttp.client3.circe.asJson

trait FinancialModelingPrepClient[F[_]]:
  //https://site.financialmodelingprep.com/developer/docs/tradable-list-api
  def getAllTradedStocks: F[List[Ticker]]

final private class LiveFinancialModelingPrepClient[F[_]](
    private val config: FinancialModelingPrepConfig,
    private val backend: SttpBackend[F, Any]
)(using
    F: Async[F]
) extends FinancialModelingPrepClient[F] {

  override def getAllTradedStocks: F[List[Ticker]] =
    backend
      .send {
        emptyRequest
          .get(uri"${config.baseUri}/api/v3/available-traded/list?apikey=${config.apiKey}")
          .response(asJson[List[FinancialModelingPrepClient.Stock]])
      }
      .flatMap { res =>
        res.body match
          case Right(stocks) => F.pure(stocks.map(_.toDomain))
          case Left(value)   => ???
      }

}

object FinancialModelingPrepClient {
  final case class Stock(
      symbol: Symbol,
      exchange: String,
      exchangeShortName: String,
      price: BigDecimal,
      name: String,
      `type`: String
  ) derives Codec.AsObject {
    def toDomain: Ticker =
      Ticker(
        symbol = symbol,
        name = name,
        price = price,
        exchange = exchange,
        exchangeShortName = exchangeShortName,
        stockType = `type`
      )
  }

  def make[F[_]](config: FinancialModelingPrepConfig, backend: SttpBackend[F, Any])(using F: Async[F]): F[FinancialModelingPrepClient[F]] =
    F.pure(LiveFinancialModelingPrepClient[F](config, backend))
}
