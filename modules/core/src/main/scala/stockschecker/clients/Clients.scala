package stockschecker.clients

import cats.effect.Async
import cats.syntax.functor.*
import stockschecker.clients.alphavantage.AlphaVantageClient
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.common.config.ClientsConfig
import stockschecker.domain.{Exchange, Ticker}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.SttpBackend

trait Clients[F[_]]:
  def marketData: MarketDataClient[F]

object Clients:
  def make[F[_]: Async](config: ClientsConfig, backend: SttpBackend[F, Fs2Streams[F]]): F[Clients[F]] =
    for
      finnhub      <- FinnhubClient.make[F](config.finnhub, backend)
      alphaVantage <- AlphaVantageClient.make(config.alphaVantage, backend)
      mdc = new MarketDataClient[F]:
        def getTradedSecurities(e: Exchange): fs2.Stream[F, domain.Security] = finnhub.getListedSecurities(e)
        def getCompanyProfile(t: Ticker): F[Option[domain.CompanyProfile]]   = finnhub.getCompanyProfile(t)
        def getMonthlyPriceCandles(t: Ticker): F[List[domain.PriceCandle]]   = alphaVantage.getMonthlyPriceCandles(t)
    yield new Clients[F]:
      def marketDataClient: MarketDataClient[F] = mdc
