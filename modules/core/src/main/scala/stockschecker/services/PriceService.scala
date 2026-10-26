package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.MarketDataClient
import stockschecker.domain.{PriceAnalytics, PriceAnalyticsFilter, PriceHistorySnapshot, Ticker}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.{CompanyProfileRepository, LatestPriceRepository, PriceAnalyticsRepository, PriceHistoryRepository}
import java.time.{Instant, LocalDate, ZoneOffset}
import java.time.temporal.ChronoUnit

trait PriceService[F[_]]:
  def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]]
  def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics]

final private class LivePriceService[F[_]](
    private val repository: PriceAnalyticsRepository[F],
    private val latestPriceRepository: LatestPriceRepository[F],
    private val marketDataClient: MarketDataClient[F],
    private val profiles: CompanyProfileRepository[F],
    private val history: PriceHistoryRepository[F]
)(using
    F: Temporal[F]
) extends PriceService[F] {

  override def findPriceAnalytics(ticker: Ticker, fetch: Boolean): F[PriceAnalytics] =
    if (fetch) fetchPriceAnalytics(ticker).flatTap(analytics => save(List(analytics)))
    else repository.find(ticker).flatMap(analytics => F.fromOption(analytics, AppError.PriceAnalyticsNotFound(ticker)))

  private def fetchPriceAnalytics(ticker: Ticker): F[PriceAnalytics] =
    for
      now <- F.realTimeInstant
      through = LocalDate.ofInstant(now, ZoneOffset.UTC)
      // Retain the existing ten-year return horizon while risk measures use a fixed five-year window.
      totalCached <- history.latestCovering(ticker, "split-and-dividend", through.minusYears(13))
      total       <- totalCached
        .filter(s => reusable(s, ticker, "split-and-dividend", through.minusYears(13), through, now))
        .fold(
          marketDataClient.getDailyPriceHistory(ticker, through.minusYears(13), through, "all").flatTap(history.save)
        )(F.pure)
      quotedCached <- history.latest(ticker, "none")
      quoted       <- quotedCached
        .filter(s => reusable(s, ticker, "none", through.minusDays(14), through, now))
        .fold(
          marketDataClient.getDailyPriceHistory(ticker, through.minusDays(14), through, "none").flatTap(history.save)
        )(F.pure)
      _ <- F.raiseWhen(
        !validIdentity(total, ticker, "split-and-dividend") || !validIdentity(quoted, ticker, "none") ||
          !total.currency.equalsIgnoreCase(quoted.currency)
      )(AppError.FailedValidation("Adjusted and quoted price histories must have matching ticker, currency and adjustment metadata"))
      bars        <- F.fromOption(NonEmptyList.fromList(total.candles), AppError.PriceAnalyticsNotFound(ticker))
      valid       <- F.fromEither(PriceAnalytics.validate(bars, monthly = false).left.map(AppError.FailedValidation(_)))
      quoteBars   <- F.fromOption(NonEmptyList.fromList(quoted.candles), AppError.PriceAnalyticsNotFound(ticker))
      validQuotes <- F.fromEither(PriceAnalytics.validate(quoteBars, monthly = false).left.map(AppError.FailedValidation(_)))
      // A deferred second request may finish in a later session. Use one common endpoint
      // for returns and the raw quote, without consuming another provider request.
      quoteDates = validQuotes.toList.map(_.date).toSet
      common <- F.fromOption(
        valid.toList.find(c => quoteDates.contains(c.date) && !c.date.isAfter(through)),
        AppError.DataUnavailable("TwelveData", ticker, "adjusted and quoted histories have no common trading session")
      )
      _ <- F.raiseWhen(ChronoUnit.DAYS.between(common.date, through) > 7)(
        AppError.DataUnavailable("TwelveData", ticker, "the latest matching trading session is more than seven days old")
      )
      latest    = validQuotes.toList.find(_.date == common.date).get
      aligned   = NonEmptyList.fromListUnsafe(valid.toList.filter(c => !c.date.isAfter(common.date)))
      analytics = PriceAnalytics.fromDaily(ticker, aligned)
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
      profiles.updatePriceAnalyticsLastUpdated(analyticsList.map(_.ticker))

  override def findPriceAnalyticsBy(filter: PriceAnalyticsFilter, limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findBy(filter, limit)

  override def getAllPriceAnalytics(limit: Option[Int]): F[List[PriceAnalytics]] =
    repository.findAll(limit)
}

object PriceService:
  def make[F[_]: Temporal](
      repository: PriceAnalyticsRepository[F],
      latestPriceRepository: LatestPriceRepository[F],
      marketDataClient: MarketDataClient[F],
      profiles: CompanyProfileRepository[F],
      history: PriceHistoryRepository[F]
  ): F[PriceService[F]] =
    Temporal[F].pure(LivePriceService[F](repository, latestPriceRepository, marketDataClient, profiles, history))
