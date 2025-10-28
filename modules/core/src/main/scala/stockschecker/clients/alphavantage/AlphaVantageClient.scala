package stockschecker.clients.alphavantage

import cats.effect.kernel.Async
import stockschecker.common.config.AlphaVantageClientConfig
import stockschecker.domain.{Candle, Ticker}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.SttpBackend

trait AlphaVantageClient[F[_]]:
  def getMonthlyPriceCandles(ticker: Ticker): F[List[Candle]]

final private class LiveAlphaVantageClient[F[_]](
    private val config: AlphaVantageClientConfig,
    private val backend: SttpBackend[F, Fs2Streams[F]]
)(using
    F: Async[F]
) extends AlphaVantageClient[F] {

  // /query?function=TIME_SERIES_MONTHLY&symbol=$ticker&apikey=${config.apiKey}
  override def getMonthlyPriceCandles(ticker: Ticker): F[List[Candle]] = ???
}

object AlphaVantageClient {
  def make[F[_]: Async](config: AlphaVantageClientConfig, backend: SttpBackend[F, Fs2Streams[F]]): F[AlphaVantageClient[F]] =
    Async[F].pure(LiveAlphaVantageClient[F](config, backend))
}
