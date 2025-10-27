package stockschecker.clients.finnhub

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import io.circe.Codec
import io.circe.fs2.{byteArrayParser, decoder}
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.errors.AppError
import stockschecker.domain.{Exchange, Security, SecurityKind, Ticker}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.*
import sttp.client3.circe.asJson

import scala.concurrent.duration.*

trait FinnhubClient[F[_]]:
  def getTradedSecurities(exchange: Exchange): Stream[F, Security]

final private class LiveFinnhubClient[F[_]](
    private val config: FinnhubClientConfig,
    private val backend: SttpBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends FinnhubClient[F] {

  override def getTradedSecurities(exchange: Exchange): Stream[F, Security] = {
    val mic  = mapExchangeToFinnhubMic(exchange)
    val code = mapExchangeToFinnhubCode(exchange)
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v1/stock/symbol?apikey=${config.apiKey}&exchange=$code&mic=$mic")
      .response(asStreamUnsafe(Fs2Streams[F]))
      .readTimeout(10.minutes)

    for
      response <- Stream.eval(backend.send(request))
      data <- response.body match
        case Right(stream) =>
          stream
            .through(byteArrayParser[F])
            .through(decoder[F, FinnhubClient.StockSymbol])
            .map(_.toDomain(exchange))
        case Left(err) =>
          Stream.raiseError(AppError.Http(response.code.code, s"Error retrieving traded stocks from finnhub: $err"))
    yield data
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
  ) derives Codec.AsObject {
    def toDomain(exchange: Exchange): Security = {
      def mapTypeToSecurityKind(stockType: String): SecurityKind = stockType match
        case "Common Stock" | "Public" => SecurityKind.Stock
        case "ETP"                     => SecurityKind.ETF
        case "REIT"                    => SecurityKind.REIT
        case "ADR"                     => SecurityKind.ADR
        case _                         => SecurityKind.Other

      Security(
        ticker = symbol,
        name = description,
        kind = mapTypeToSecurityKind(`type`),
        exchange = exchange
      )
    }
  }

  def make[F[_]: Async](
      config: FinnhubClientConfig,
      backend: SttpBackend[F, Fs2Streams[F]]
  ): F[FinnhubClient[F]] =
    Async[F].pure(LiveFinnhubClient[F](config, backend))
}
