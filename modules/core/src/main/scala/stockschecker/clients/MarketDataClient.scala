package stockschecker.clients

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.clients.finnhub.FinnhubClient
import stockschecker.clients.twelvedata.TwelveDataClient
import stockschecker.repositories.{DataQualityRepository, ProviderBudgetRepository}
import java.time.LocalDate

trait MarketDataClient[F[_]]:
  def getTradedSecurities(exchange: Exchange): Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
  def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]]
  def getFinancialMetrics(ticker: Ticker): F[Option[FinancialMetrics]]
  def getFinancialData(ticker: Ticker): F[Option[FinancialData]]
  def getDailyPriceHistory(ticker: Ticker, from: LocalDate, through: LocalDate, adjustment: String): F[PriceHistorySnapshot]

object MarketDataClient {
  def make[F[_]: Async](
      finnhub: FinnhubClient[F],
      twelve: TwelveDataClient[F],
      quality: Option[DataQualityRepository[F]] = None,
      finnhubLimits: ProviderLimits = ProviderLimits(),
      twelveLimits: ProviderLimits = ProviderLimits(8, 800, 1),
      providerBudgets: Option[ProviderBudgetRepository[F]] = None
  ): F[MarketDataClient[F]] =
    for
      fh <- ProviderAccess.make[F]("Finnhub", finnhubLimits, providerBudgets)
      td <- ProviderAccess.make[F]("TwelveData", twelveLimits, providerBudgets)
    yield new MarketDataClient[F] {
      private val F                                                                                                           = Async[F]
      private def record(ticker: Ticker, provider: String, dataset: String, status: String, failure: Option[String]): F[Unit] =
        quality.fold(F.unit)(repo => F.realTimeInstant.flatMap(now => repo.record(ticker, provider, dataset, status, now, failure)))
      private def observed[A](ticker: Ticker, provider: String, dataset: String)(request: F[A])(present: A => Boolean): F[A] =
        request.attempt.flatMap {
          case Right(value) => record(ticker, provider, dataset, if (present(value)) "available" else "unavailable", None).as(value)
          case Left(error)  =>
            val status = error match
              case _: AppError.ProviderDeferred              => "deferred"
              case _: AppError.ProviderConfiguration         => "configuration-error"
              case e: AppError.HttpClient if e.status == 404 => "unavailable"
              case _                                         => "failed"
            val summary = error match
              case e: AppError.ProviderDeferred      => s"Deferred until ${e.retryAt}"
              case _: AppError.ProviderConfiguration => "Check provider credentials and subscription"
              case e: AppError.HttpClient            => s"HTTP ${e.status}"
              case _: AppError.JsonParsingFailure    => "Invalid provider response"
              case _                                 => error.getClass.getSimpleName
            record(ticker, provider, dataset, status, Some(summary)) >> F.raiseError(error)
        }
      def getTradedSecurities(exchange: Exchange): Stream[F, Security] =
        Stream.eval(fh.run(finnhub.getListedSecurities(exchange).compile.toList)).flatMap(Stream.emits)
      def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
        observed(ticker, "Finnhub", "profile")(fh.run(finnhub.getCompanyProfile(ticker)))(_.nonEmpty)
      def getFinancialMetrics(ticker: Ticker): F[Option[FinancialMetrics]] =
        observed(ticker, "Finnhub", "financials")(fh.run(finnhub.getFinancialMetrics(ticker)))(_.nonEmpty)
      def getFinancialData(ticker: Ticker): F[Option[FinancialData]] =
        observed(ticker, "Finnhub", "financials")(fh.run(finnhub.getFinancialData(ticker)))(_.nonEmpty)
      def getMonthlyPriceCandles(ticker: Ticker): F[NonEmptyList[PriceCandle]] =
        observed(ticker, "TwelveData", "monthly-prices")(td.run(twelve.getMonthlyPriceCandles(ticker)))(_ => true)
      def getDailyPriceHistory(ticker: Ticker, from: LocalDate, through: LocalDate, adjustment: String): F[PriceHistorySnapshot] =
        observed(ticker, "TwelveData", s"daily-prices-$adjustment")(td.run(twelve.getDailyPriceHistory(ticker, from, through, adjustment)))(
          _.candles.nonEmpty
        )
    }
}
