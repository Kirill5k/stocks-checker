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
import stockschecker.domain.{CompanyProfile, Stock, Ticker}
import sttp.client3.*
import sttp.client3.circe.asJson
import sttp.capabilities.fs2.Fs2Streams
import sttp.model.StatusCode
import fs2.Stream
import sttp.capabilities.WebSockets

import java.time.{Instant, LocalDate}

final private class FinancialModelingPrepClient[F[_]](
    private val config: FinancialModelingPrepConfig,
    private val backend: SttpBackend[F, Fs2Streams[F] & WebSockets]
)(using
    F: Async[F],
    C: Clock[F]
) extends MarketDataClient[F] {

  override def getAllTradedStocks: Stream[F, Stock] =
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v3/available-traded/list?apikey=${config.apiKey}")
      .response(asStreamUnsafe(Fs2Streams[F]))

    for
      time     <- Stream.eval(C.now)
      response <- Stream.eval(backend.send(request))
      data <- response.body match
        case Right(stream) =>
          stream
            .through(byteArrayParser[F])
            .through(decoder[F, FinancialModelingPrepClient.StockResponse])
            .filter(_.isValid)
            .map(_.toDomain(time))
        case Left(err) =>
          Stream.raiseError(AppError.Http(response.code.code, s"Error retrieving traded stocks from financial modeling prep: $err"))
    yield data

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
        case Left(HttpError(b, s)) if s == StatusCode.NotFound =>
          F.pure(None)
        case Left(HttpError(b, s)) =>
          F.raiseError(AppError.Http(s.code, s"Error retrieving company profile: $b"))
    yield res
  }
}

object FinancialModelingPrepClient {
  final case class StockResponse(
      symbol: Ticker,
      exchange: Option[String],
      exchangeShortName: Option[String],
      price: BigDecimal,
      name: Option[String],
      `type`: String
  ) derives Codec.AsObject {
    def isValid: Boolean = exchange.isDefined && name.isDefined
    def toDomain(lastUpdatedAt: Instant): Stock =
      Stock(
        ticker = symbol,
        name = name.getOrElse(""),
        price = price,
        exchange = exchange.getOrElse(""),
        exchangeShortName = exchangeShortName.getOrElse(""),
        stockType = `type`,
        lastUpdatedAt = lastUpdatedAt
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
      ipoDate: LocalDate,
      currency: String,
      price: BigDecimal,
      mktCap: Long,
      volAvg: Long,
      isEtf: Boolean,
      isActivelyTrading: Boolean,
      isFund: Boolean,
      isAdr: Boolean
  ) derives Codec.AsObject {
    def toDomain(time: Instant): CompanyProfile =
      CompanyProfile(
        ticker = symbol,
        name = companyName,
        country = country,
        sector = sector,
        industry = industry,
        description = description,
        website = website,
        ipoDate = ipoDate,
        currency = currency,
        stockPrice = price,
        marketCap = mktCap,
        averageTradedVolume = volAvg,
        isEtf = isEtf,
        isActivelyTrading = isActivelyTrading,
        isFund = isFund,
        isAdr = isAdr,
        lastUpdatedAt = time
      )
  }

  def make[F[_]: Clock: Async](
      config: FinancialModelingPrepConfig,
      backend: SttpBackend[F, Fs2Streams[F] & WebSockets]
  ): F[MarketDataClient[F]] =
    Async[F].pure(FinancialModelingPrepClient[F](config, backend))
}
