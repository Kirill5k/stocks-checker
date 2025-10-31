package stockschecker.clients.alphavantage

import cats.data.NonEmptyList
import cats.effect.kernel.{Async, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.{Decoder, HCursor}
import stockschecker.common.config.AlphaVantageClientConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.*
import sttp.client3.circe.asJson

import java.time.LocalDate
import scala.collection.immutable.ListMap

trait AlphaVantageClient[F[_]]:
  def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]]

final private class LiveAlphaVantageClient[F[_]](
    private val config: AlphaVantageClientConfig,
    private val apiKeys: Array[String],
    private val keyIndex: Ref[F, Int],
    private val backend: SttpBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends AlphaVantageClient[F] {

  private def getNextApiKey: F[String] =
    keyIndex.modify { currentIndex =>
      val nextIndex = (currentIndex + 1) % apiKeys.length
      (nextIndex, apiKeys(currentIndex))
    }

  override def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]] =
    for {
      apiKey <- getNextApiKey
      request = emptyRequest
        .get(uri"${config.baseUri}/query?function=TIME_SERIES_MONTHLY&symbol=$ticker&apikey=$apiKey")
        .response(asJson[AlphaVantageClient.MonthlyTimeSeriesResponse])
      response <- backend.send(request)
      result   <- response.body match
        case Right(data) =>
          // Check for error messages in Note or Information fields first
          val errorMessage = data.note.orElse(data.information)
          errorMessage match
            case Some(msg) =>
              // Determine status code based on error message content
              val statusCode = if msg.contains("API rate limit") then 429 else 500
              F.raiseError(AppError.Http(statusCode, s"AlphaVantage API error: $msg"))
            case None =>
              // No error message, check for time series data
              data.timeSeries match
                case Some(series) if series.isEmpty =>
                  F.raiseError(AppError.Http(500, s"No price candle data returned for ticker ${ticker.value}"))
                case Some(series) =>
                  val candles = series.map { case (dateStr, candle) => candle.toDomain(LocalDate.parse(dateStr)) }.toList
                  F.pure(NonEmptyList.fromListUnsafe(candles))
                case None =>
                  F.raiseError(AppError.Http(500, s"No time series data returned for ticker ${ticker.value}"))
        case Left(err) =>
          F.raiseError(AppError.Http(response.code.code, s"Error retrieving monthly price candles: ${err.getMessage}"))
    } yield result
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

  def make[F[_]](config: AlphaVantageClientConfig, backend: SttpBackend[F, Fs2Streams[F]])(using F: Async[F]): F[AlphaVantageClient[F]] =
    val apiKeys = config.apiKey.split(',').map(_.trim).filter(_.nonEmpty)
    F.raiseWhen(apiKeys.isEmpty)(AppError.Critical("At least one AlphaVantage API key must be provided")) >>
      Ref.of[F, Int](0).map(keyIndex => LiveAlphaVantageClient[F](config, apiKeys, keyIndex, backend))
}
