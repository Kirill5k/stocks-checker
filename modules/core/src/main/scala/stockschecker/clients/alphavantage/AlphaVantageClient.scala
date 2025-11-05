package stockschecker.clients.alphavantage

import cats.data.NonEmptyList
import cats.effect.kernel.{Async, Ref}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.{Decoder, HCursor}
import stockschecker.common.config.AlphaVantageClientConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson

import java.time.LocalDate
import scala.collection.immutable.ListMap

trait AlphaVantageClient[F[_]]:
  def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]]

final private class LiveAlphaVantageClient[F[_]](
    private val config: AlphaVantageClientConfig,
    private val apiKeys: Array[String],
    private val keyIndex: Ref[F, Int],
    private val backend: WebSocketStreamBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends AlphaVantageClient[F] {

  private def getNextApiKey: F[String] =
    keyIndex.modify { currentIndex =>
      val nextIndex = (currentIndex + 1) % apiKeys.length
      (nextIndex, apiKeys(currentIndex))
    }

  override def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    attemptWithKeyRotation(ticker, Set.empty)

  private def attemptWithKeyRotation(ticker: Ticker, triedKeys: Set[String]): F[NonEmptyList[PriceCandle]] =
    for
      apiKey <- getNextApiKey
      _      <- F.raiseWhen(triedKeys.contains(apiKey) && triedKeys.size >= apiKeys.length) {
        AppError.Http(429, s"All Alpha Vantage API keys exhausted due to rate limiting: ${triedKeys.mkString(",")}")
      }
      result <- sendRequest(ticker, apiKey).recoverWith {
        case err: AppError.Http if err.status == 429 => attemptWithKeyRotation(ticker, triedKeys + apiKey)
      }
    yield result

  private def sendRequest(ticker: Ticker, apiKey: String): F[NonEmptyList[PriceCandle]] =
    for
      response <- backend.send {
        emptyRequest
          .get(uri"${config.baseUri}/query?function=TIME_SERIES_MONTHLY&symbol=$ticker&apikey=$apiKey")
          .response(asJson[AlphaVantageClient.MonthlyTimeSeriesResponse])
      }
      result <- response.body match
        case Right(data) =>
          processData(data, ticker)
        case Left(ResponseException.DeserializationException(responseBody, error, _)) =>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"Alpha Vantage client returned ${error.getMessage}"))
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) =>
          F.raiseError(AppError.Http(meta.code.code, s"Alpha Vantage client returned unexpected status ${meta.code.code} with body: $body"))
    yield result

  private def processData(data: AlphaVantageClient.MonthlyTimeSeriesResponse, ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    data.timeSeries match
      case Some(series) if series.nonEmpty =>
        val candles = series.map { case (dateStr, candle) => candle.toDomain(LocalDate.parse(dateStr)) }.toList
        F.pure(NonEmptyList.fromListUnsafe(candles))
      case _ =>
        handleError(data.note.orElse(data.information), ticker)

  private def handleError(errorMessage: Option[String], ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    errorMessage match
      case Some(msg) =>
        val statusCode = if msg.contains("API rate limit") then 429 else 500
        F.raiseError(AppError.Http(statusCode, s"AlphaVantage API error: $msg"))
      case None =>
        F.raiseError(AppError.Http(500, s"No time series data returned for ticker ${ticker.value}"))
}

object AlphaVantageClient {
  final case class CandleData(
      open: String,  // "1. open"
      high: String,  // "2. high"
      low: String,   // "3. low"
      close: String, // "4. close"
      volume: String // "5. volume"
  ) {
    def toDomain(date: LocalDate): PriceCandle =
      PriceCandle(
        date = date,
        open = BigDecimal(open),
        high = BigDecimal(high),
        low = BigDecimal(low),
        close = BigDecimal(close),
        volume = volume.toLong
      )
  }

  object CandleData {
    given Decoder[CandleData] = (c: HCursor) =>
      for
        open   <- c.downField("1. open").as[String]
        high   <- c.downField("2. high").as[String]
        low    <- c.downField("3. low").as[String]
        close  <- c.downField("4. close").as[String]
        volume <- c.downField("5. volume").as[String]
      yield CandleData(open, high, low, close, volume)
  }

  final case class MonthlyTimeSeriesResponse(
      timeSeries: Option[ListMap[String, CandleData]], // "Monthly Time Series"
      information: Option[String],                     // "Information" field for errors
      note: Option[String]                             // "Note" field for errors
  )

  object MonthlyTimeSeriesResponse {
    given Decoder[MonthlyTimeSeriesResponse] = (c: HCursor) =>
      for
        timeSeries  <- c.downField("Monthly Time Series").as[Option[ListMap[String, CandleData]]]
        information <- c.downField("Information").as[Option[String]]
        note        <- c.downField("Note").as[Option[String]]
      yield MonthlyTimeSeriesResponse(timeSeries, information, note)
  }

  def make[F[_]](
      config: AlphaVantageClientConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  )(using F: Async[F]): F[AlphaVantageClient[F]] =
    val apiKeys = config.apiKey.split(',').map(_.trim).filter(_.nonEmpty)
    F.raiseWhen(apiKeys.isEmpty)(AppError.Critical("At least one AlphaVantage API key must be provided")) >>
      Ref.of[F, Int](0).map(keyIndex => LiveAlphaVantageClient[F](config, apiKeys, keyIndex, backend))
}
