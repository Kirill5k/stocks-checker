package stockschecker.clients.alphavantage

import cats.data.NonEmptyList
import cats.effect.kernel.{Async, Ref}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.either.*
import io.circe.{Decoder, DecodingFailure, HCursor}
import stockschecker.common.config.AlphaVantageClientConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson

import java.time.LocalDate
import scala.collection.immutable.ListMap
import scala.concurrent.duration.*

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

  private def claimStartIndex: F[Int] =
    keyIndex.getAndUpdate(i => (i + 1) % apiKeys.length)

  override def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    claimStartIndex.flatMap(startIndex => attemptWithKeyRotation(ticker, startIndex, Set.empty))

  private def attemptWithKeyRotation(ticker: Ticker, startIndex: Int, triedKeys: Set[String]): F[NonEmptyList[PriceCandle]] =
    if triedKeys.size >= apiKeys.length then
      F.raiseError(AppError.HttpClient("AlphaVantage", 429, s"All API keys exhausted due to rate limiting: ${triedKeys.mkString(",")}"))
    else
      val apiKey = apiKeys((startIndex + triedKeys.size) % apiKeys.length)
      sendRequest(ticker, apiKey).recoverWith {
        case err: AppError.HttpClient if err.status == 429 =>
          attemptWithKeyRotation(ticker, startIndex, triedKeys + apiKey)
      }

  private def sendRequest(ticker: Ticker, apiKey: String): F[NonEmptyList[PriceCandle]] =
    for
      response <- backend.send {
        emptyRequest
          .get(uri"${config.baseUri}/query?function=TIME_SERIES_MONTHLY&symbol=$ticker&apikey=$apiKey")
          .response(asJson[AlphaVantageClient.MonthlyTimeSeriesResponse])
          .readTimeout(30.seconds)
      }
      result <- response.body match
        case Right(data) =>
          processData(data, ticker)
        case Left(ResponseException.DeserializationException(responseBody, error, _)) =>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"Alpha Vantage client returned ${error.getMessage}"))
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) =>
          F.raiseError(AppError.HttpClient("AlphaVantage", meta.code.code, body))
    yield result

  private def processData(data: AlphaVantageClient.MonthlyTimeSeriesResponse, ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    data.timeSeries
      .map(series => series.map { case (dateStr, candle) => candle.toDomain(LocalDate.parse(dateStr)) }.toList)
      .flatMap(NonEmptyList.fromList)
      .fold(handleError(data.note.orElse(data.information), ticker))(F.pure)

  private def handleError(errorMessage: Option[String], ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    errorMessage match
      case Some(msg) =>
        val statusCode = if msg.contains("API rate limit") then 429 else 500
        F.raiseError(AppError.HttpClient("AlphaVantage", statusCode, s"API error: $msg"))
      case None =>
        F.raiseError(AppError.HttpClient("AlphaVantage", 500, s"No time series data returned for ticker $ticker"))
}

object AlphaVantageClient {
  final case class CandleData(
      open: BigDecimal,
      high: BigDecimal,
      low: BigDecimal,
      close: BigDecimal,
      volume: Long
  ) {
    def toDomain(date: LocalDate): PriceCandle =
      PriceCandle(date = date, open = open, high = high, low = low, close = close, volume = volume)
  }

  object CandleData {
    private def parseDecimal(s: String, field: String): Decoder.Result[BigDecimal] =
      Either
        .catchNonFatal(BigDecimal(s))
        .leftMap(e => DecodingFailure(s"Invalid decimal '$s' for field '$field': ${e.getMessage}", Nil))

    private def parseLong(s: String, field: String): Decoder.Result[Long] =
      Either
        .catchNonFatal(s.toLong)
        .leftMap(e => DecodingFailure(s"Invalid long '$s' for field '$field': ${e.getMessage}", Nil))

    given Decoder[CandleData] = (c: HCursor) =>
      for
        open   <- c.downField("1. open").as[String].flatMap(parseDecimal(_, "1. open"))
        high   <- c.downField("2. high").as[String].flatMap(parseDecimal(_, "2. high"))
        low    <- c.downField("3. low").as[String].flatMap(parseDecimal(_, "3. low"))
        close  <- c.downField("4. close").as[String].flatMap(parseDecimal(_, "4. close"))
        volume <- c.downField("5. volume").as[String].flatMap(parseLong(_, "5. volume"))
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
