package stockschecker.clients

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
  def resumeProvider(provider: String): F[Unit]
  def getTradedSecurities(exchange: Exchange): Stream[F, Security]
  def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]]
  def getFinancialData(ticker: Ticker): F[Option[FinancialData]]
  def getDailyPriceHistory(ticker: Ticker, from: LocalDate, through: LocalDate, adjustment: String): F[PriceHistorySnapshot]

object MarketDataClient {
  def make[F[_]: Async](
      finnhub: FinnhubClient[F],
      twelve: TwelveDataClient[F],
      quality: DataQualityRepository[F],
      finnhubLimits: ProviderLimits,
      twelveLimits: ProviderLimits,
      providerBudgets: ProviderBudgetRepository[F]
  ): F[MarketDataClient[F]] =
    for
      fh <- ProviderAccess.make[F]("Finnhub", finnhubLimits, providerBudgets)
      td <- ProviderAccess.make[F]("TwelveData", twelveLimits, providerBudgets)
    yield new MarketDataClient[F] {
      private val F                                                                                                           = Async[F]
      private def record(ticker: Ticker, provider: String, dataset: String, status: String, failure: Option[String]): F[Unit] =
        F.realTimeInstant.flatMap(now => quality.record(ticker, provider, dataset, status, now, failure))
      private def observed[A](ticker: Ticker, provider: String, dataset: String)(request: F[A])(present: A => Boolean): F[A] =
        request.attempt.flatMap {
          case Right(value) => record(ticker, provider, dataset, if (present(value)) "available" else "unavailable", None).as(value)
          case Left(error: AppError.ProviderDeferred) => F.raiseError(error)
          case Left(error)                            =>
            val status = error match
              case _: AppError.ProviderConfiguration         => "configuration-error"
              case e: AppError.HttpClient if e.status == 404 => "unavailable"
              case _                                         => "failed"
            record(ticker, provider, dataset, status, Some(ProviderFailure.summary(error))) >> F.raiseError(error)
        }
      def resumeProvider(provider: String): F[Unit] = provider.toLowerCase(java.util.Locale.ROOT) match
        case "finnhub"    => fh.resume
        case "twelvedata" => td.resume
        case _            => F.raiseError(AppError.FailedValidation("Unknown provider; expected Finnhub or TwelveData"))
      def getTradedSecurities(exchange: Exchange): Stream[F, Security] =
        Stream.eval(fh.run(finnhub.getListedSecurities(exchange).compile.toList)).flatMap(Stream.emits)
      def getCompanyProfile(ticker: Ticker): F[Option[CompanyProfile]] =
        observed(ticker, "Finnhub", "profile")(fh.run(finnhub.getCompanyProfile(ticker)))(_.nonEmpty)
      def getFinancialData(ticker: Ticker): F[Option[FinancialData]] =
        observed(ticker, "Finnhub", "financials")(fh.run(finnhub.getFinancialData(ticker)))(_.nonEmpty)
      def getDailyPriceHistory(ticker: Ticker, from: LocalDate, through: LocalDate, adjustment: String): F[PriceHistorySnapshot] =
        observed(ticker, "TwelveData", s"daily-prices-$adjustment")(td.run(twelve.getDailyPriceHistory(ticker, from, through, adjustment)))(
          _.candles.nonEmpty
        )
    }
}
