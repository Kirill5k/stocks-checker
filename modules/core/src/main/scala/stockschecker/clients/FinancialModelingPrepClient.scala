package stockschecker.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import io.circe.fs2.*
import kirill5k.common.cats.Clock
import stockschecker.clients.FinancialModelingPrepClient.CompanyProfileResponse
import stockschecker.common.config.FinancialModelingPrepConfig
import stockschecker.domain.errors.AppError
import stockschecker.domain.{CompanyProfile, SecurityType, StockQuote, Ticker}
import sttp.client3.*
import sttp.client3.circe.asJson
import sttp.capabilities.fs2.Fs2Streams
import sttp.model.StatusCode
import fs2.Stream

import java.time.{Instant, LocalDate}
import scala.concurrent.duration.*

final private class FinancialModelingPrepClient[F[_]](
    private val config: FinancialModelingPrepConfig,
    private val backend: SttpBackend[F, Fs2Streams[F]]
)(using
    F: Async[F],
    C: Clock[F]
) extends MarketDataClient[F] {

  override def getAllTradedStocks: Stream[F, StockQuote] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v3/available-traded/list?apikey=${config.apiKey}")
      .response(asStreamUnsafe(Fs2Streams[F]))
      .readTimeout(10.minutes)

    for
      time     <- Stream.eval(C.now)
      response <- Stream.eval(backend.send(request))
      data <- response.body match
        case Right(stream) =>
          stream
            .through(byteArrayParser[F])
            .through(decoder[F, FinancialModelingPrepClient.StockResponse])
            .filter(s => s.isRegularStock)
            .map(_.toDomain(time))
        case Left(err) =>
          Stream.raiseError(AppError.Http(response.code.code, s"Error retrieving traded stocks from financial modeling prep: $err"))
    yield data
  }

  override def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v3/profile/$ticker?apikey=${config.apiKey}")
      .response(asJson[List[CompanyProfileResponse]])

    for
      time     <- C.now
      response <- backend.send(request)
      res <- response.body match
        case Right(Nil) =>
          F.pure(None)
        case Right(companyProfile :: _) =>
          F.pure(Some(companyProfile.toDomain(time)))
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
  final case class StockResponse(
      symbol: Ticker,
      price: BigDecimal,
      `type`: String,
      exchangeShortName: Option[String] = None
  ) derives Codec.AsObject {
    def isRegularStock: Boolean = !symbol.value.contains(".")
    def toDomain(quotedAt: Instant): StockQuote =
      StockQuote(
        ticker = symbol,
        price = price,
        quotedAt = quotedAt,
        createdAt = quotedAt
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

    private def parse52WeekRange: (Option[BigDecimal], Option[BigDecimal]) =
      range.flatMap { r =>
        r.split("-").toList match {
          case low :: high :: Nil =>
            for {
              l <- scala.util.Try(BigDecimal(low.trim)).toOption
              h <- scala.util.Try(BigDecimal(high.trim)).toOption
            } yield (Some(l), Some(h))
          case _ => None
        }
      }.getOrElse((None, None))

    private def parseEmployees: Option[Long] =
      fullTimeEmployees.flatMap(s => scala.util.Try(s.toLong).toOption)

    private def determineSecurityType: SecurityType =
      if (isEtf) SecurityType.ETF
      else if (isFund) SecurityType.Fund
      else if (isAdr) SecurityType.ADR
      else SecurityType.CommonStock

    def toDomain(time: Instant): CompanyProfile = {
      val (week52Low, week52High) = parse52WeekRange
      CompanyProfile(
        ticker = symbol,
        name = companyName,
        securityType = determineSecurityType,
        exchange = exchangeShortName,
        currency = currency,
        country = Some(country).filter(_.nonEmpty),
        sector = Some(sector).filter(_.nonEmpty),
        industry = Some(industry).filter(_.nonEmpty),
        description = Some(description).filter(_.nonEmpty),
        website = Some(website).filter(_.nonEmpty),
        ipoDate = ipoDate,
        ceo = ceo.filter(_.nonEmpty),
        employees = parseEmployees,
        marketCap = mktCap,
        averageVolume = volAvg,
        week52High = week52High,
        week52Low = week52Low,
        isActivelyTrading = isActivelyTrading,
        createdAt = time,
        updatedAt = time
      )
    }
  }

  def make[F[_]: Clock: Async](
      config: FinancialModelingPrepConfig,
      backend: SttpBackend[F, Fs2Streams[F]]
  ): F[MarketDataClient[F]] =
    Async[F].pure(FinancialModelingPrepClient[F](config, backend))
}
