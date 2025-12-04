package stockschecker.clients.finnhub

import cats.effect.Async
import cats.syntax.flatMap.*
import fs2.Stream
import io.circe.{Codec, JsonObject}
import io.circe.fs2.{byteArrayParser, decoder}
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, Exchange, Security, SecurityKind, Ticker}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.model.StatusCode

import java.time.LocalDate
import scala.concurrent.duration.*
import scala.util.Try

trait FinnhubClient[F[_]]:
  def getListedSecurities(exchange: Exchange): Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]

final private class LiveFinnhubClient[F[_]](
    private val config: FinnhubClientConfig,
    private val backend: WebSocketStreamBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends FinnhubClient[F] {

  override def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v1/stock/profile2?token=${config.apiKey}&symbol=$ticker")
      .response(asJson[JsonObject])

    backend.send(request).flatMap { response =>
      response.body match
        case Right(jsonObj) if jsonObj.isEmpty =>
          F.pure(None) // Empty object returned - no profile found
        case Right(jsonObj) =>
          jsonObj.toJson.as[FinnhubClient.CompanyProfileResponse] match
            case Right(profile) =>
              F.pure(Some(profile.toDomain))
            case Left(err) =>
              F.raiseError(AppError.JsonParsingFailure(jsonObj.toString, s"Error decoding company profile for $ticker: ${err.getMessage}"))
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) if meta.code == StatusCode.TooManyRequests =>
          F.sleep(2.second) >> getCompanyProfile(ticker)
        case Left(err) =>
          F.raiseError(AppError.Http(response.code.code, s"Error retrieving company profile for $ticker: ${err.getMessage}"))
    }
  }

  override def getListedSecurities(exchange: Exchange): Stream[F, Security] = {
    val mic     = mapExchangeToFinnhubMic(exchange)
    val code    = mapExchangeToFinnhubCode(exchange)
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v1/stock/symbol?token=${config.apiKey}&exchange=$code&mic=$mic")
      .response(asStreamUnsafe(Fs2Streams[F]))
      .readTimeout(10.minutes)

    for
      response <- Stream.eval(backend.send(request))
      data     <- response.body match
        case Right(stream) =>
          stream
            .through(byteArrayParser[F])
            .through(decoder[F, FinnhubClient.StockSymbol])
            .map(_.toDomain(exchange))
        case Left(err) =>
          Stream.raiseError(AppError.Http(response.code.code, s"Error retrieving traded stocks from finnhub: $err"))
    yield data
  }

  private def mapExchangeToFinnhubCode(exchange: Exchange): String = exchange match
    case Exchange.NASDAQ => "US"
    case Exchange.NYSE   => "US"

  private def mapExchangeToFinnhubMic(exchange: Exchange): String = exchange match
    case Exchange.NASDAQ => "XNAS"
    case Exchange.NYSE   => "XNYS"

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

  final case class CompanyProfileResponse(
      ticker: Ticker,
      name: String,
      country: String,
      finnhubIndustry: String,
      description: Option[String],
      weburl: String,
      ipo: String,
      estimateCurrency: String,
      marketCapitalization: BigDecimal
  ) derives Codec.AsObject {
    def toDomain: CompanyProfile =
      CompanyProfile(
        ticker = ticker,
        name = name.toUpperCase,
        country = country,
        industry = finnhubIndustry,
        description = description,
        website = weburl,
        ipoDate = if (ipo.trim.isEmpty) None else Try(LocalDate.parse(ipo)).toOption,
        currency = estimateCurrency,
        marketCap = (marketCapitalization * 1_000_000).longValue
      )
  }

  def make[F[_]: Async](
      config: FinnhubClientConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  ): F[FinnhubClient[F]] =
    Async[F].pure(LiveFinnhubClient[F](config, backend))
}
