package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PriceAnalytics, PriceAnalyticsFilter, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{LatestPriceRepository, PriceAnalyticsRepository, PriceHistoryRepository}
import java.time.{Instant, LocalDate, ZoneOffset}
import java.time.temporal.ChronoUnit

trait PriceService[F[_]]:
  def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics]
  def fetchLatestPriceAnalytics(tickers: NonEmptyList[Ticker]): F[Unit]

final private class LivePriceService[F[_]](
    private val repository: PriceAnalyticsRepository[F],
    private val latestPriceRepository: LatestPriceRepository[F],
    private val marketDataClient: MarketDataClient[F],
    private val dispatcher: ActionDispatcher[F],
    private val history: Option[PriceHistoryRepository[F]]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends PriceService[F] {

  override def fetchLatestPriceAnalytics(tickers: NonEmptyList[Ticker]): F[Unit] =
    Stream
      .emits(tickers.toList)
      .evalMap(ticker =>
        findPriceAnalytics(ticker, fetch = true).void.onError { case error =>
          logger.error(error)(s"Price refresh failed for $ticker")
        }.attempt
      )
      .compile
      .toList
      .flatMap { outcomes =>
        outcomes.collectFirst { case Left(error) => error }.fold(F.unit)(F.raiseError[Unit])
      }

  override def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics] =
    if (fetch) fetchPriceAnalytics(ticker).flatTap(analytics => save(List(analytics)))
    else repository.find(ticker).flatMap(analytics => F.fromOption(analytics, AppError.PriceAnalyticsNotFound(ticker)))

  private def fetchPriceAnalytics(ticker: Ticker): F[PriceAnalytics] = history match
    case None =>
      for
        candles <- marketDataClient.getMonthlyPriceCandles(ticker)
        valid   <- F.fromEither(PriceAnalytics.validate(candles).left.map(AppError.FailedValidation(_)))
      yield PriceAnalytics.from(ticker, valid)
    case Some(repo) =>
      for
        now <- F.realTimeInstant
        through = LocalDate.ofInstant(now, ZoneOffset.UTC)
        // Retain the existing ten-year return horizon while risk measures use a fixed five-year window.
        totalCached <- repo.latest(ticker, "split-and-dividend")
        total       <- totalCached
          .filter(s => reusable(s, ticker, "split-and-dividend", through.minusYears(13), through, now))
          .fold(
            marketDataClient.getDailyPriceHistory(ticker, through.minusYears(13), through, "all").flatTap(repo.save)
          )(F.pure)
        quotedCached <- repo.latest(ticker, "none")
        quoted       <- quotedCached
          .filter(s => reusable(s, ticker, "none", through.minusDays(14), through, now))
          .fold(
            marketDataClient.getDailyPriceHistory(ticker, through.minusDays(14), through, "none").flatTap(repo.save)
          )(F.pure)
        _ <- F.raiseWhen(
          !validIdentity(total, ticker, "split-and-dividend") || !validIdentity(quoted, ticker, "none") ||
            !total.currency.equalsIgnoreCase(quoted.currency)
        )(AppError.FailedValidation("Adjusted and quoted price histories must have matching ticker, currency and adjustment metadata"))
        bars        <- F.fromOption(NonEmptyList.fromList(total.candles), AppError.PriceAnalyticsNotFound(ticker))
        valid       <- F.fromEither(PriceAnalytics.validate(bars, monthly = false).left.map(AppError.FailedValidation(_)))
        quoteBars   <- F.fromOption(NonEmptyList.fromList(quoted.candles), AppError.PriceAnalyticsNotFound(ticker))
        validQuotes <- F.fromEither(PriceAnalytics.validate(quoteBars, monthly = false).left.map(AppError.FailedValidation(_)))
        latest = validQuotes.head
        _ <- F.raiseWhen(
          latest.date != valid.head.date || latest.date.isAfter(through) || ChronoUnit.DAYS.between(latest.date, through) > 7
        )(AppError.FailedValidation("Adjusted and quoted price histories must end on the same recent trading date"))
        analytics = PriceAnalytics.fromDaily(ticker, valid)
      yield analytics.copy(
        performanceSummary = analytics.performanceSummary.copy(latestPrice = latest.close, latestPriceDate = latest.date),
        basis = analytics.basis.map(_.copy(priceObservationId = Some(total.id), quotedPriceObservationId = Some(quoted.id)))
      )

  private def validIdentity(snapshot: PriceHistorySnapshot, ticker: Ticker, adjustment: String): Boolean =
    snapshot.ticker == ticker && snapshot.adjustment == adjustment && snapshot.currency.trim.nonEmpty

  private def reusable(
      snapshot: PriceHistorySnapshot,
      ticker: Ticker,
      adjustment: String,
      from: LocalDate,
      through: LocalDate,
      now: Instant
  ): Boolean =
    validIdentity(snapshot, ticker, adjustment) && !snapshot.fetchedAt.isAfter(now) && snapshot.fetchedAt.isAfter(
      now.minusSeconds(86400)
    ) &&
      !snapshot.requestedFrom.isAfter(from) && !snapshot.requestedThrough.isBefore(through)

  private def save(analyticsList: List[PriceAnalytics]): F[Unit] =
    repository.save(analyticsList) >>
      latestPriceRepository.save(analyticsList.map(_.toLatestPrice)) >>
      dispatcher.dispatch(Action.RecordPriceAnalyticsUpdate(analyticsList.map(_.ticker)))

  override def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findBy(filter, limit)

  override def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findAll(limit)
}

object PriceService:
  def make[F[_]: {Temporal, Logger}](
      repository: PriceAnalyticsRepository[F],
      latestPriceRepository: LatestPriceRepository[F],
      marketDataClient: MarketDataClient[F],
      dispatcher: ActionDispatcher[F],
      history: Option[PriceHistoryRepository[F]] = None
  ): F[PriceService[F]] =
    Temporal[F].pure(LivePriceService[F](repository, latestPriceRepository, marketDataClient, dispatcher, history))
