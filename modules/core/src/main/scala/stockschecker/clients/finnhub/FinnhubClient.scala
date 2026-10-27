package stockschecker.clients.finnhub

import stockschecker.domain.Provider
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import io.circe.{Codec, JsonObject}
import io.circe.fs2.{byteArrayParser, decoder}
import stockschecker.clients.{ProviderAccess, ProviderErrors}
import kirill5k.common.cats.Clock
import stockschecker.common.config.FinnhubClientConfig
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, Exchange, FinancialData, FinancialMetrics, Security, SecurityKind, Ticker}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson

import java.time.LocalDate
import scala.concurrent.duration.*
import scala.util.Try

trait FinnhubClient[F[_]]:
  def getListedSecurities(exchange: Exchange): Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
  def getFinancialData(ticker: Ticker): F[Option[FinancialData]]

final private class LiveFinnhubClient[F[_]](
    private val config: FinnhubClientConfig,
    private val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val access: ProviderAccess[F]
)(using
    F: Async[F],
    C: Clock[F]
) extends FinnhubClient[F] {

  override def getFinancialData(ticker: Ticker): F[Option[FinancialData]] =
    access.run(Provider.Finnhub, config.limits)(fetchFinancialData(ticker))

  override def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
    access.run(Provider.Finnhub, config.limits)(fetchCompanyProfile(ticker))

  override def getListedSecurities(exchange: Exchange): Stream[F, Security] =
    Stream.eval(access.run(Provider.Finnhub, config.limits)(fetchListedSecurities(exchange).compile.toList)).flatMap(Stream.emits)

  private def fetchFinancialData(ticker: Ticker): F[Option[FinancialData]] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v1/stock/metric?token=${config.apiKey}&symbol=$ticker&metric=all")
      .response(asJson[JsonObject])
      .readTimeout(30.seconds)

    backend.send(request).flatMap { response =>
      handle(response, s"basic financials for $ticker") { body =>
        C.now.flatMap { now =>
          F.fromEither(
            FinnhubFinancialMapping
              .decode(body, ticker, now)
              .left
              .map(err => AppError.JsonParsingFailure(body.toString, s"Error decoding financial metrics for $ticker: ${err.getMessage}"))
          )
        }
      }
    }
  }

  private def fetchCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v1/stock/profile2?token=${config.apiKey}&symbol=$ticker")
      .response(asJson[JsonObject])
      .readTimeout(30.seconds)

    backend.send(request).flatMap { response =>
      handle(response, s"company profile for $ticker") { jsonObj =>
        if jsonObj.isEmpty then F.pure(None)
        else
          F.fromEither(
            jsonObj.toJson
              .as[FinnhubClient.CompanyProfileResponse]
              .map(profile => Some(profile.toDomain))
              .left
              .map(err => AppError.JsonParsingFailure(jsonObj.toString, s"Error decoding company profile for $ticker: ${err.getMessage}"))
          )
      }
    }
  }

  private def fetchListedSecurities(exchange: Exchange): Stream[F, Security] = {
    val mic     = mapExchangeToFinnhubMic(exchange)
    val code    = mapExchangeToFinnhubCode(exchange)
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v1/stock/symbol?token=${config.apiKey}&exchange=$code&mic=$mic")
      .response(asStreamUnsafe(Fs2Streams[F]))
      .readTimeout(10.minutes)

    Stream.eval(backend.send(request).flatMap(response => handle(response, s"listed securities for $exchange")(F.pure))).flatMap { stream =>
      stream
        .through(byteArrayParser[F])
        .through(decoder[F, FinnhubClient.StockSymbol])
        .handleErrorWith {
          case error: io.circe.Error =>
            Stream.raiseError[F](
              AppError.JsonParsingFailure("", s"Error decoding Finnhub listed securities for $exchange: ${error.getMessage}")
            )
          case error => Stream.raiseError[F](error)
        }
        .map(_.toDomain(exchange))
    }
  }

  private def handle[E, A, B](response: Response[Either[E, A]], context: String)(onSuccess: A => F[B]): F[B] =
    if !response.code.isSuccess then
      ProviderErrors
        .fromStatus[F](Provider.Finnhub, response.code.code, response.header("Retry-After"), s"Error retrieving Finnhub $context")
        .flatMap(F.raiseError)
    else
      response.body match
        case Right(body)                                                      => onSuccess(body)
        case Left(ResponseException.DeserializationException(body, error, _)) =>
          F.raiseError(AppError.JsonParsingFailure(body, s"Error decoding Finnhub $context: ${error.getMessage}"))
        case Left(_) => F.raiseError(AppError.JsonParsingFailure("", s"Error decoding Finnhub $context"))

  private def mapExchangeToFinnhubCode(exchange: Exchange): String = exchange match
    case Exchange.NASDAQ => "US"
    case Exchange.NYSE   => "US"

  private def mapExchangeToFinnhubMic(exchange: Exchange): String = exchange match
    case Exchange.NASDAQ => "XNAS"
    case Exchange.NYSE   => "XNYS"

}

object FinnhubClient {
  final case class FinancialMetricsResponse(
      `10DayAverageTradingVolume`: Option[BigDecimal],
      `52WeekHigh`: Option[BigDecimal],
      `52WeekLow`: Option[BigDecimal],
      `52WeekPriceReturnDaily`: Option[BigDecimal],
      beta: Option[BigDecimal],
      peTTM: Option[BigDecimal],
      epsTTM: Option[BigDecimal],
      roeTTM: Option[BigDecimal],
      dividendYieldIndicatedAnnual: Option[BigDecimal],
      `totalDebt/totalEquityAnnual`: Option[BigDecimal],
      netProfitMarginTTM: Option[BigDecimal],
      cashFlowPerShareTTM: Option[BigDecimal],
      revenueGrowth5Y: Option[BigDecimal],
      epsGrowth5Y: Option[BigDecimal]
  ) derives Codec.AsObject:
    def toDomain(ticker: Ticker): FinancialMetrics =
      FinancialMetrics(
        ticker = ticker,
        peRatioTtm = peTTM,
        epsTtm = epsTTM,
        roeTtm = roeTTM,
        dividendYieldAnnual = dividendYieldIndicatedAnnual,
        debtToEquityAnnual = `totalDebt/totalEquityAnnual`,
        profitMarginTtm = netProfitMarginTTM,
        freeCashFlowPerShareTtm = None,
        revenueGrowth5Y = revenueGrowth5Y,
        epsGrowth5Y = epsGrowth5Y,
        priceHigh52Week = `52WeekHigh`,
        priceLow52Week = `52WeekLow`
      )

  final case class BasicFinancialsResponse(
      metric: JsonObject
  ) derives Codec.AsObject

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
        marketCap = (marketCapitalization * 1_000_000).longValue,
        sector = FinnhubSectorMapping.sectorFor(finnhubIndustry)
      )
  }

  def make[F[_]: {Async, Clock}](
      config: FinnhubClientConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      access: ProviderAccess[F]
  ): F[FinnhubClient[F]] =
    access.validateLimits(Provider.Finnhub, config.limits).as(LiveFinnhubClient[F](config, backend, access))
}
