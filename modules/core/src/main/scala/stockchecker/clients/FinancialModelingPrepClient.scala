package stockchecker.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import io.circe.Codec
import stockchecker.common.config.FinancialModelingPrepConfig
import stockchecker.domain.errors.AppError
import stockchecker.domain.{Stock, Ticker}
import sttp.client3.*
import sttp.client3.circe.asJson

trait FinancialModelingPrepClient[F[_]]:
  // https://site.financialmodelingprep.com/developer/docs/tradable-list-api
  // available alternative: https://finnhub.io/docs/api/stock-symbols
  def getAllTradedStocks: F[List[Stock]]

final private class LiveFinancialModelingPrepClient[F[_]](
    private val config: FinancialModelingPrepConfig,
    private val backend: SttpBackend[F, Any]
)(using
    F: Async[F]
) extends FinancialModelingPrepClient[F] {

  override def getAllTradedStocks: F[List[Stock]] =
    backend
      .send {
        emptyRequest
          .get(uri"${config.baseUri}/api/v3/available-traded/list?apikey=${config.apiKey}")
          .response(asJson[List[FinancialModelingPrepClient.StockResponse]])
      }
      .flatMap { res =>
        res.body match
          case Right(stocks) =>
            F.pure(stocks.map(_.toDomain))
          case Left(DeserializationException(_, error)) =>
            F.raiseError(AppError.Json(s"Failed to deserialize available traded stocks response: ${error}"))
          case Left(HttpError(b, s)) =>
            F.raiseError(AppError.Http(s.code, s"Error retrieving traded stocks: $b"))
      }

}

object FinancialModelingPrepClient {
  final case class StockResponse(
      symbol: Ticker,
      exchange: String,
      exchangeShortName: String,
      price: BigDecimal,
      name: String,
      `type`: String
  ) derives Codec.AsObject {
    def toDomain: Stock =
      Stock(
        ticker = symbol,
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
