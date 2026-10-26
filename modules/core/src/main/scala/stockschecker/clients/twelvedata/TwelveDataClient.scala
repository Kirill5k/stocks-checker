package stockschecker.clients.twelvedata

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import stockschecker.common.config.TwelveDataConfig
import stockschecker.domain.{PriceCandle, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.clients.RetryAfter
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson

import java.time.LocalDate
import scala.concurrent.duration.*

trait TwelveDataClient[F[_]]:
  def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]]
  def getDailyPriceHistory(ticker: Ticker, from: LocalDate, through: LocalDate, adjustment: String): F[PriceHistorySnapshot]

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
          .get(uri"${config.baseUri}/time_series?symbol=$ticker&interval=1month&apikey=${config.apiKey}&outputsize=150&adjust=splits")
          .response(asJson[TwelveDataClient.TimeSeriesResponse])
          .readTimeout(30.seconds)
      }
      result <- response.body match
        case Right(data) =>
          processData(data, ticker)
        case Left(ResponseException.DeserializationException(responseBody, error, _)) =>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"TwelveData client returned ${error.getMessage}"))
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) =>
          failure(meta.code.code, body, meta.header("Retry-After"))
    yield result

  override def getDailyPriceHistory(ticker: Ticker, from: LocalDate, through: LocalDate, adjustment: String): F[PriceHistorySnapshot] =
    for
      _ <- F.raiseWhen(!Set("all", "none", "splits").contains(adjustment) || from.isAfter(through))(
        AppError.FailedValidation("Invalid price history request")
      )
      response <- backend.send(
        emptyRequest
          .get(
            uri"${config.baseUri}/time_series?symbol=$ticker&interval=1day&apikey=${config.apiKey}&outputsize=5000&start_date=$from&end_date=$through&adjust=$adjustment"
          )
          .response(asJson[TwelveDataClient.TimeSeriesResponse])
          .readTimeout(30.seconds)
      )
      data <- response.body match
        case Right(value)                                             => F.pure(value)
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) => failure(meta.code.code, body, meta.header("Retry-After"))
        case Left(error) => F.raiseError(AppError.JsonParsingFailure("", s"Invalid daily prices: ${error.getMessage}"))
      values <- processData(data, ticker)
      now    <- F.realTimeInstant
      candles = values.toList.sortBy(_.date)
      _ <- F.raiseWhen(candles.exists(_.close <= 0) || candles.map(_.date).distinct.size != candles.size)(
        AppError.FailedValidation("Price history contains invalid prices or duplicate dates")
      )
      _ <- F.raiseWhen(candles.exists(c => c.date.isBefore(from) || c.date.isAfter(through)) || candles.size >= 5000)(
        AppError.FailedValidation("Price history is outside the requested range or may be truncated")
      )
      _ <- F.raiseWhen(candles.sliding(2).exists {
        case List(older, newer) => java.time.temporal.ChronoUnit.DAYS.between(older.date, newer.date) > 7
        case _                  => false
      })(AppError.FailedValidation("Daily price history contains an unexplained gap longer than seven calendar days"))
      meta <- F.fromOption(data.meta, AppError.FailedValidation("Price history has no currency metadata"))
      _    <- F.raiseWhen(meta.symbol != ticker.value || meta.interval != "1day")(
        AppError.FailedValidation("Price history identity/interval mismatch")
      )
    yield PriceHistorySnapshot(
      java.util.UUID.randomUUID().toString,
      ticker,
      candles,
      "TwelveData",
      meta.currency,
      if (adjustment == "all") "split-and-dividend" else adjustment,
      now,
      from,
      through
    )

  private def failure[A](status: Int, message: String, retryAfter: Option[String] = None): F[A] = status match
    case 429 =>
      F.realTimeInstant.flatMap(now =>
        F.raiseError(AppError.ProviderDeferred("TwelveData", RetryAfter.deadline(retryAfter, now), "rate-limited"))
      )
    case 401 | 403 => F.raiseError(AppError.ProviderConfiguration("TwelveData", s"HTTP $status; check credentials and subscription"))
    case _         => F.raiseError(AppError.HttpClient("TwelveData", status, message))

  private def processData(data: TwelveDataClient.TimeSeriesResponse, ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    if data.isOk then
      val maybeValues = data.values.flatMap(v => NonEmptyList.fromList(v.map(_.toDomain)))
      F.fromOption(maybeValues, AppError.HttpClient("TwelveData", 500, s"No values returned for ticker $ticker"))
    else if data.isError then
      val errorCode    = data.code.getOrElse(500)
      val errorMessage = data.message.getOrElse("Unknown error from TwelveData API")
      failure(errorCode, s"API error for getting time series data for $ticker: $errorMessage")
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
