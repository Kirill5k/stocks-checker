package stockschecker.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import io.circe.fs2.*
import stockschecker.clients.FinancialModelingPrepClient.CompanyProfileResponse
import stockschecker.common.config.FinancialModelingPrepConfig
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, Exchange, Security, SecurityKind, Ticker}
import sttp.client3.*
import sttp.client3.circe.asJson
import sttp.capabilities.fs2.Fs2Streams
import sttp.model.StatusCode
import fs2.Stream

import java.time.LocalDate
import scala.concurrent.duration.*

final private class FinancialModelingPrepClient[F[_]](
    private val config: FinancialModelingPrepConfig,
    private val backend: SttpBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends MarketDataClient[F] {

  override def getAllTradedSecurities: Stream[F, Security] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v3/available-traded/list?apikey=${config.apiKey}")
      .response(asStreamUnsafe(Fs2Streams[F]))
      .readTimeout(10.minutes)

    for
      response <- Stream.eval(backend.send(request))
      data <- response.body match
        case Right(stream) =>
          stream
            .through(byteArrayParser[F])
            .through(decoder[F, FinancialModelingPrepClient.SecurityResponse])
            .filter(s => s.isRegularStock)
            .map(_.toDomain)
        case Left(err) =>
          Stream.raiseError(AppError.Http(response.code.code, s"Error retrieving traded stocks from financial modeling prep: $err"))
    yield data
  }

  override def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v3/profile/$ticker?apikey=${config.apiKey}")
      .response(asJson[List[CompanyProfileResponse]])

    for
      response <- backend.send(request)
      res <- response.body match
        case Right(Nil) =>
          F.pure(None)
        case Right(companyProfile :: _) =>
          F.pure(Some(companyProfile.toDomain))
        case Left(DeserializationException(body, error)) =>
          F.raiseError(AppError.JsonParsingFailure(body, s"Failed to deserialize company profile response: ${error}"))
        case Left(HttpError(_, s)) if s == StatusCode.NotFound =>
          F.pure(None)
        case Left(HttpError(b, s)) =>
          F.raiseError(AppError.Http(s.code, s"Error retrieving company profile: $b"))
    yield res
  }
}

object FinancialModelingPrepClient {
  final case class SecurityResponse(
      symbol: Ticker,
      price: BigDecimal,
      `type`: String,
      exchangeShortName: Option[String] = None
  ) derives Codec.AsObject {
    def isRegularStock: Boolean = !symbol.value.contains(".")

    private def mapExchange(shortName: String): Option[Exchange] = shortName match
      case "NSQ" | "NASDAQ" => Some(Exchange.NASDAQ)
      case "NYS" | "NYSE"   => Some(Exchange.NYSE)
      case _                => None

    private def mapSecurityKind(typeStr: String): SecurityKind = typeStr.toLowerCase match
      case s if s.contains("etf")         => SecurityKind.ETF
      case s if s.contains("mutual fund") => SecurityKind.MutualFund
      case s if s.contains("reit")        => SecurityKind.REIT
      case s if s.contains("adr")         => SecurityKind.ADR
      case _                              => SecurityKind.Stock

    def toDomain: Security =
      Security(
        ticker = symbol,
        exchange = exchangeShortName.flatMap(mapExchange).getOrElse(Exchange.NASDAQ),
        name = symbol.value, // Using ticker as name since name is not provided in this response
        kind = mapSecurityKind(`type`),
        isActive = true
      )
  }

  final case class CompanyProfileResponse(
      symbol: Ticker,
      companyName: String,
      country: String,
      sector: String,
      industry: String,
      description: String,
      website: String,
      exchangeShortName: Option[String],
      ipoDate: Option[LocalDate],
      currency: String,
      price: BigDecimal,
      mktCap: Option[Long],
      volAvg: Option[Long],
      range: Option[String],
      ceo: Option[String],
      fullTimeEmployees: Option[String],
      isEtf: Boolean,
      isActivelyTrading: Boolean,
      isFund: Boolean,
      isAdr: Boolean
  ) derives Codec.AsObject {

    def toDomain: CompanyProfile = {
      CompanyProfile(
        name = companyName,
        country = country,
        sector = sector,
        industry = industry,
        description = description,
        website = website,
        ipoDate = ipoDate.getOrElse(LocalDate.now()),
        currency = currency,
        marketCap = mktCap.getOrElse(0L)
      )
    }
  }

  def make[F[_]: Async](
      config: FinancialModelingPrepConfig,
      backend: SttpBackend[F, Fs2Streams[F]]
  ): F[MarketDataClient[F]] =
    Async[F].pure(FinancialModelingPrepClient[F](config, backend))
}
