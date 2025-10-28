package stockschecker.clients.alphavantage

import cats.effect.kernel.Async
import cats.syntax.flatMap.*
import io.circe.{Decoder, HCursor}
import stockschecker.common.config.AlphaVantageClientConfig
import stockschecker.domain.{PriceCandle, Ticker}
import stockschecker.domain.errors.AppError
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.*
import sttp.client3.circe.asJson

import java.time.LocalDate

trait AlphaVantageClient[F[_]]:
  def getMonthlyPriceCandles(ticker: Ticker): F[List[PriceCandle]]

final private class LiveAlphaVantageClient[F[_]](
    private val config: AlphaVantageClientConfig,
    private val backend: SttpBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends AlphaVantageClient[F] {

  override def getMonthlyPriceCandles(ticker: Ticker): F[List[PriceCandle]] = {
    val request = emptyRequest
      .get(uri"${config.baseUri}/query?function=TIME_SERIES_MONTHLY&symbol=$ticker&apikey=${config.apiKey}")
      .response(asJson[AlphaVantageClient.MonthlyTimeSeriesResponse])

    backend.send(request).flatMap { response =>
      response.body match
        case Right(data) =>
          data.timeSeries match
            case Some(series) =>
              F.pure(series.map { case (dateStr, candle) => candle.toDomain(LocalDate.parse(dateStr)) }.toList)
            case None =>
              data.information match
                case Some(info) =>
                  F.raiseError(AppError.Http(429, s"AlphaVantage API error: $info"))
                case None =>
                  F.raiseError(AppError.Http(response.code.code, "No time series data returned"))
        case Left(err) =>
          F.raiseError(AppError.Http(response.code.code, s"Error retrieving monthly price candles: ${err.getMessage}"))
    }
  }
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
      for {
        open   <- c.downField("1. open").as[String]
        high   <- c.downField("2. high").as[String]
        low    <- c.downField("3. low").as[String]
        close  <- c.downField("4. close").as[String]
        volume <- c.downField("5. volume").as[String]
      } yield CandleData(open, high, low, close, volume)
  }

  final case class MonthlyTimeSeriesResponse(
      timeSeries: Option[Map[String, CandleData]], // "Monthly Time Series"
      information: Option[String]                  // "Information" field for errors
  )

  object MonthlyTimeSeriesResponse {
    given Decoder[MonthlyTimeSeriesResponse] = (c: HCursor) =>
      for {
        timeSeries  <- c.downField("Monthly Time Series").as[Option[Map[String, CandleData]]]
        information <- c.downField("Information").as[Option[String]]
      } yield MonthlyTimeSeriesResponse(timeSeries, information)
  }

  def make[F[_]: Async](config: AlphaVantageClientConfig, backend: SttpBackend[F, Fs2Streams[F]]): F[AlphaVantageClient[F]] =
    Async[F].pure(LiveAlphaVantageClient[F](config, backend))
}
