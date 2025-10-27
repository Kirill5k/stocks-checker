package stockschecker.clients.finnhub

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Exchange, Security, SecurityKind, Ticker}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.*
import sttp.client3.circe.asJson

trait FinnhubClient[F[_]]:
  def getTradedSecurities(exchange: Exchange): F[List[Security]]

final private class LiveFinnhubClient[F[_]](
    private val config: FinnhubClientConfig,
    private val backend: SttpBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends FinnhubClient[F] {

  override def getTradedSecurities(exchange: Exchange): F[List[Security]] = {
    val mic  = mapExchangeToFinnhubMic(exchange)
    val code = mapExchangeToFinnhubCode(exchange)
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v1/stock/symbol?apikey=${config.apiKey}&exchange=$code&mic=$mic")
      .response(asJson[List[FinnhubClient.StockSymbol]])

    for
      response <- backend.send(request)
      res <- response.body match
        case Right(stockSymbols) => F.pure(stockSymbols.map(mapToSecurity(exchange)))
        case Left(DeserializationException(body, error)) =>
          F.raiseError(AppError.JsonParsingFailure(body, s"Failed to deserialize stock symbol response: ${error}"))
        case Left(HttpError(b, s)) =>
          F.raiseError(AppError.Http(s.code, s"Error retrieving stock symbols: $b"))
    yield res
  }

  private def mapExchangeToFinnhubCode(exchange: Exchange): String =
    exchange match {
      case Exchange.NASDAQ => "US"
      case Exchange.NYSE   => "US"
    }

  private def mapExchangeToFinnhubMic(exchange: Exchange): String =
    exchange match {
      case Exchange.NASDAQ => "XNAS"
      case Exchange.NYSE   => "XNYS"
    }

  private def mapTypeToSecurityKind(stockType: String): SecurityKind =
    stockType match {
      case "Common Stock" | "Public" => SecurityKind.Stock
      case "ETP"                     => SecurityKind.ETF
      case "REIT"                    => SecurityKind.REIT
      case "ADR"                     => SecurityKind.ADR
      case _                         => SecurityKind.Other
    }

  private def mapToSecurity(exchange: Exchange)(stockSymbol: FinnhubClient.StockSymbol): Security =
    Security(
      ticker = stockSymbol.symbol,
      name = stockSymbol.description,
      kind = mapTypeToSecurityKind(stockSymbol.`type`),
      exchange = exchange
    )
}

object FinnhubClient {
  final case class StockSymbol(
      currency: String,
      description: String,
      displaySymbol: String,
      figi: String,
      mic: String,
      symbol: Ticker,
      `type`: String
  ) derives Codec.AsObject

  def make[F[_]: Async](
      config: FinnhubClientConfig,
      backend: SttpBackend[F, Fs2Streams[F]]
  ): F[FinnhubClient[F]] =
    Async[F].pure(LiveFinnhubClient[F](config, backend))
}
