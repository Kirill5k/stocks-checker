package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PricePerformanceSummary, Ticker}
import stockschecker.repositories.PricePerformanceSummaryRepository

trait PriceService[F[_]]:
  def findPerformanceSummary(ticker: Ticker): F[PricePerformanceSummary]
  def fetchLatestPerformanceSummary(ticker: Ticker): F[Unit]

final private class LivePriceService[F[_]](
    private val repository: PricePerformanceSummaryRepository[F],
    private val marketDataClient: MarketDataClient[F]
)(using F: MonadThrow[F]) extends PriceService[F] {

  override def fetchLatestPerformanceSummary(ticker: Ticker): F[Unit] =
    marketDataClient
        .getMonthlyPriceCandles(ticker)
        .flatMap(candles => repository.save(PricePerformanceSummary.from(ticker, candles)))

  override def findPerformanceSummary(ticker: Ticker): F[PricePerformanceSummary] =
    repository.find(ticker).flatMap(unfoldOpt(F.pure, fetchPerformanceSummary(ticker)))

  private def unfoldOpt[A](ifPresent: A => F[A], ifMissing: => F[A])(opt: Option[A]): F[A] =
    opt match
      case Some(value) => ifPresent(value)
      case None        => ifMissing

  private def fetchPerformanceSummary(ticker: Ticker): F[PricePerformanceSummary] =
    marketDataClient
      .getMonthlyPriceCandles(ticker)
      .flatMap { candles =>
        val summary = PricePerformanceSummary.from(ticker, candles)
        repository.save(summary).as(summary)
      }
}

object PriceService:
  def make[F[_]](
      repository: PricePerformanceSummaryRepository[F],
      marketDataClient: MarketDataClient[F]
  )(using MonadThrow[F]): F[PriceService[F]] =
    MonadThrow[F].pure(LivePriceService[F](repository, marketDataClient))
