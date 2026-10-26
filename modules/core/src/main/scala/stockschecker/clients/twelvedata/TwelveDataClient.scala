package stockschecker.clients.twelvedata

import cats.data.NonEmptyList
import cats.effect.kernel.{Async, Ref}
import cats.syntax.all.*
import io.circe.Codec
import stockschecker.common.config.TwelveDataConfig
import stockschecker.domain.{DateRange, PriceAdjustment, PriceCandle, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.clients.{ProviderAccess, RetryAfter}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.LocalDate
import java.util.HexFormat
import scala.concurrent.duration.*

trait TwelveDataClient[F[_]]:
  def getDailyPriceHistory(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment): F[PriceHistorySnapshot]

final private class LiveTwelveDataClient[F[_]](
    private val config: TwelveDataConfig,
    private val apiKeys: Vector[(String, String)],
    private val keyIndex: Ref[F, Int],
    private val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val access: ProviderAccess[F]
)(using
    F: Async[F]
) extends TwelveDataClient[F] {

  override def getDailyPriceHistory(ticker: Ticker, range: DateRange, adjustment: PriceAdjustment): F[PriceHistorySnapshot] =
    for
      _      <- F.raiseWhen(!range.isValid)(AppError.FailedValidation("Invalid price history request"))
      result <- access.observed(ticker, "TwelveData", s"daily-prices-${providerAdjustment(adjustment)}")(
        keyIndex.getAndUpdate(i => (i + 1) % apiKeys.size).flatMap { startIndex =>
          attemptWithKeyRotation(ticker, range, adjustment, startIndex, 0, List.empty)
        }
      )(_.candles.nonEmpty)
    yield result

  private def attemptWithKeyRotation(
      ticker: Ticker,
      range: DateRange,
      adjustment: PriceAdjustment,
      startIndex: Int,
      attempted: Int,
      failures: List[Throwable]
  ): F[PriceHistorySnapshot] =
    if attempted >= apiKeys.size then
      val earliest = failures.collect { case error: AppError.ProviderDeferred => error }.minByOption(_.retryAt)
      F.raiseError(earliest.getOrElse(failures.head))
    else
      val (apiKey, account) = apiKeys((startIndex + attempted) % apiKeys.size)
      access.run("TwelveData", config.limits, Some(account))(sendRequest(ticker, range, adjustment, apiKey)).recoverWith {
        case error: AppError.ProviderDeferred =>
          attemptWithKeyRotation(ticker, range, adjustment, startIndex, attempted + 1, error :: failures)
        case error: AppError.ProviderConfiguration =>
          attemptWithKeyRotation(ticker, range, adjustment, startIndex, attempted + 1, error :: failures)
      }

  private def sendRequest(
      ticker: Ticker,
      range: DateRange,
      adjustment: PriceAdjustment,
      apiKey: String
  ): F[PriceHistorySnapshot] =
    for
      queryParams = Map(
        "symbol"     -> ticker.value,
        "interval"   -> "1day",
        "apikey"     -> apiKey,
        "outputsize" -> "5000",
        "start_date" -> range.from.toString,
        "end_date"   -> range.through.toString,
        "adjust"     -> providerAdjustment(adjustment)
      )
      response <- backend.send(
        emptyRequest
          .get(uri"${config.baseUri}/time_series".addParams(queryParams))
          .response(asJson[TwelveDataClient.TimeSeriesResponse])
          .readTimeout(30.seconds)
      )
      data <- response.body match
        case Right(value)                                             => F.pure(value)
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) => failure(meta.code.code, body, meta.header("Retry-After"))
        case Left(error) => F.raiseError(AppError.JsonParsingFailure("", s"Invalid daily prices: ${redact(error.getMessage)}"))
      values <- processData(data, ticker)
      now    <- F.realTimeInstant
      candles = values.toList.sortBy(_.date)
      _ <- F.raiseWhen(candles.exists(_.close <= 0) || candles.map(_.date).distinct.size != candles.size)(
        AppError.FailedValidation("Price history contains invalid prices or duplicate dates")
      )
      _ <- F.raiseWhen(candles.exists(c => !range.contains(c.date)) || candles.size >= 5000)(
        AppError.FailedValidation("Price history is outside the requested range or may be truncated")
      )
      // Calendar gaps can be valid exchange closures or trading halts. Consumers validate
      // the dates actually needed for their return horizons instead of rejecting the whole vintage.
      meta <- F.fromOption(data.meta, AppError.FailedValidation("Price history has no currency metadata"))
      _    <- F.raiseWhen(meta.symbol != ticker.value || meta.interval != "1day" || meta.currency.trim.isEmpty)(
        AppError.FailedValidation("Price history identity/interval mismatch")
      )
    yield PriceHistorySnapshot(
      java.util.UUID.randomUUID().toString,
      ticker,
      candles,
      "TwelveData",
      meta.currency,
      adjustment,
      now,
      range.from,
      range.through
    )

  private def providerAdjustment(adjustment: PriceAdjustment): String = adjustment match
    case PriceAdjustment.Unadjusted         => "none"
    case PriceAdjustment.Splits             => "splits"
    case PriceAdjustment.SplitsAndDividends => "all"

  private def redact(message: String): String =
    apiKeys.map(_._1).sortBy(key => -key.length).foldLeft(message)((result, key) => result.replace(key, "[redacted]"))

  private def failure[A](status: Int, message: String, retryAfter: Option[String] = None): F[A] = status match
    case 429 =>
      F.realTimeInstant.flatMap(now =>
        F.raiseError(AppError.ProviderDeferred("TwelveData", RetryAfter.deadline(retryAfter, now), "rate-limited"))
      )
    case 401 | 403 => F.raiseError(AppError.ProviderConfiguration("TwelveData", s"HTTP $status; check credentials and subscription"))
    case _         => F.raiseError(AppError.HttpClient("TwelveData", status, redact(message)))

  private def processData(data: TwelveDataClient.TimeSeriesResponse, ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    if data.isOk then
      val maybeValues = data.values.flatMap(v => NonEmptyList.fromList(v.map(_.toDomain)))
      F.fromOption(maybeValues, AppError.HttpClient("TwelveData", 500, s"No values returned for ticker $ticker"))
    else if data.isError then
      val errorCode    = data.code.getOrElse(500)
      val errorMessage = data.message.getOrElse("Unknown error from TwelveData API")
      failure(errorCode, s"API error for getting time series data for $ticker: $errorMessage")
    else
      F.raiseError(
        AppError.HttpClient(
          "TwelveData",
          500,
          s"Time series API returned unexpected status code for ticker $ticker: ${redact(data.toString)}"
        )
      )
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
      backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      access: ProviderAccess[F]
  )(using F: Async[F]): F[TwelveDataClient[F]] =
    val keys = config.apiKey.split(',').iterator.map(_.trim).filter(_.nonEmpty).toVector.distinct
    for
      _        <- F.raiseWhen(keys.isEmpty)(AppError.Critical("At least one TwelveData API key must be provided"))
      _        <- access.validateLimits("TwelveData", config.limits)
      accounts <- F.delay(keys.map { key =>
        val digest = MessageDigest.getInstance("SHA-256").digest(key.getBytes(StandardCharsets.UTF_8))
        key -> HexFormat.of().formatHex(digest)
      })
      index <- Ref.of[F, Int](0)
    yield LiveTwelveDataClient[F](config, accounts, index, backend, access)
}
