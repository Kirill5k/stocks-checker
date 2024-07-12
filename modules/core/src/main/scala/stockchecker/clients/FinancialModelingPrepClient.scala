package stockchecker.clients

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import kirill5k.common.cats.Clock
import stockchecker.clients.FinancialModelingPrepClient.CompanyProfileResponse
import stockchecker.common.config.FinancialModelingPrepConfig
import stockchecker.domain.errors.AppError
import stockchecker.domain.{CompanyProfile, Stock, Ticker}
import sttp.client3.*
import sttp.client3.circe.asJson
import sttp.model.StatusCode

import java.time.{Instant, LocalDate}

trait FinancialModelingPrepClient[F[_]]:
  // https://site.financialmodelingprep.com/developer/docs/tradable-list-api
  // available alternative: https://finnhub.io/docs/api/stock-symbols
  def getAllTradedStocks: F[List[Stock]]
  // https://site.financialmodelingprep.com/developer/docs/companies-key-stats-free-api
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]

final private class LiveFinancialModelingPrepClient[F[_]](
    private val config: FinancialModelingPrepConfig,
    private val backend: SttpBackend[F, Any]
)(using
    F: Async[F],
    C: Clock[F]
) extends FinancialModelingPrepClient[F] {

  override def getAllTradedStocks: F[List[Stock]] =
    val request = emptyRequest
      .get(uri"${config.baseUri}/api/v3/available-traded/list?apikey=${config.apiKey}")
      .response(asJson[List[FinancialModelingPrepClient.StockResponse]])

    for
      time     <- C.now
      response <- backend.send(request)
      res <- response.body match
        case Right(stocks) =>
          F.pure(stocks.map(_.toDomain(time)))
        case Left(DeserializationException(_, error)) =>
          F.raiseError(AppError.Json(s"Failed to deserialize available traded stocks response: ${error}"))
        case Left(HttpError(b, s)) =>
          F.raiseError(AppError.Http(s.code, s"Error retrieving traded stocks: $b"))
    yield res

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
        case Left(DeserializationException(_, error)) =>
          F.raiseError(AppError.Json(s"Failed to deserialize company profile response: ${error}"))
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
      exchange: String,
      exchangeShortName: String,
      price: BigDecimal,
      name: String,
      `type`: String
  ) derives Codec.AsObject {
    def toDomain(lastUpdatedAt: Instant): Stock =
      Stock(
        ticker = symbol,
        name = name,
        price = price,
        exchange = exchange,
        exchangeShortName = exchangeShortName,
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

  def make[F[_]: Clock: Async](config: FinancialModelingPrepConfig, backend: SttpBackend[F, Any]): F[FinancialModelingPrepClient[F]] =
    Async[F].pure(LiveFinancialModelingPrepClient[F](config, backend))
}
