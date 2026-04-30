package stockschecker.clients.twelvedata

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import stockschecker.common.config.TwelveDataConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson

import java.time.LocalDate
import scala.concurrent.duration.*

trait TwelveDataClient[F[_]]:
  def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]]

final private class LiveTwelveDataClient[F[_]](
    private val config: TwelveDataConfig,
    private val backend: WebSocketStreamBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends TwelveDataClient[F] {

  override def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    for
      response <- backend.send {
        emptyRequest
          .get(uri"${config.baseUri}/time_series?symbol=$ticker&interval=1month&apikey=${config.apiKey}&outputsize=150")
          .response(asJson[TwelveDataClient.TimeSeriesResponse])
          .readTimeout(30.seconds)
      }
      result <- response.body match
        case Right(data) =>
          processData(data, ticker)
        case Left(ResponseException.DeserializationException(responseBody, error, _)) =>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"TwelveData client returned ${error.getMessage}"))
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) =>
          F.raiseError(AppError.HttpClient("TwelveData", meta.code.code, body))
    yield result

  private def processData(data: TwelveDataClient.TimeSeriesResponse, ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    if data.isOk then
      val maybeValues = data.values.flatMap(v => NonEmptyList.fromList(v.map(_.toDomain)))
      F.fromOption(maybeValues, AppError.HttpClient("TwelveData", 500, s"No values returned for ticker $ticker"))
    else if data.isError then
      val errorCode    = data.code.getOrElse(500)
      val errorMessage = data.message.getOrElse("Unknown error from TwelveData API")
      F.raiseError(AppError.HttpClient("TwelveData", errorCode, s"API error for getting time series data for $ticker: $errorMessage"))
    else F.raiseError(AppError.HttpClient("TwelveData", 500, s"Time series API returned unexpected status code for ticker $ticker: $data"))
}

object TwelveDataClient {
  final case class CandleData(
      datetime: LocalDate,
      open: BigDecimal,
      high: BigDecimal,
      low: BigDecimal,
      close: BigDecimal,
      volume: BigDecimal
  ) derives Codec.AsObject {
    def toDomain: PriceCandle =
      PriceCandle(
        date = datetime,
        open = open,
        high = high,
        low = low,
        close = close,
        volume = volume.toLong
      )
  }

  final case class Meta(
      symbol: String,
      interval: String,
      currency: String,
      exchange_timezone: String,
      exchange: String,
      mic_code: String,
      `type`: String
  ) derives Codec.AsObject

  final case class TimeSeriesResponse(
      meta: Option[Meta],
      values: Option[List[CandleData]],
      status: String,
      code: Option[Int],
      message: Option[String]
  ) derives Codec.AsObject {
    def isOk: Boolean    = status == "ok"
    def isError: Boolean = status == "error"
  }

  def make[F[_]](
      config: TwelveDataConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  )(using F: Async[F]): F[TwelveDataClient[F]] =
    F.pure(LiveTwelveDataClient[F](config, backend))
}
