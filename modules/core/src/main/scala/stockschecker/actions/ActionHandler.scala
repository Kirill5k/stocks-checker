package stockschecker.actions

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.functor.*
import stockschecker.domain.{JobPlan, Ticker}
import stockschecker.services.{
  CompanyProfileService,
  FinancialMetricsService,
  InvestmentEvaluationService,
  InvestmentRankingService,
  InvestmentScreenService,
  PriceService,
  SecurityService,
  Services
}

enum ActionResult:
  case Completed
  case Expand(plan: JobPlan)
  case Cancelled(reason: String)

trait ActionHandler[F[_]]:
  def execute(action: Action): F[ActionResult]

final private class LiveActionHandler[F[_]](
    security: SecurityService[F],
    profiles: CompanyProfileService[F],
    prices: PriceService[F],
    financials: FinancialMetricsService[F],
    screens: InvestmentScreenService[F],
    rankings: InvestmentRankingService[F],
    evaluations: InvestmentEvaluationService[F]
)(using F: Monad[F])
    extends ActionHandler[F]:
  private def completed(effect: F[Unit]): F[ActionResult] =
    effect.as(ActionResult.Completed)
  private def expand(actions: List[Action], sequential: Boolean = false): ActionResult =
    ActionResult.Expand(JobPlan(actions, sequential))

  private def perTicker(tickers: List[Ticker])(action: NonEmptyList[Ticker] => Action): ActionResult =
    expand(tickers.map(ticker => action(NonEmptyList.one(ticker))))

  private def update(tickers: NonEmptyList[Ticker])(action: NonEmptyList[Ticker] => Action)(run: Ticker => F[Unit]): F[ActionResult] =
    if tickers.tail.isEmpty then completed(run(tickers.head))
    else F.pure(perTicker(tickers.toList)(action))

  override def execute(action: Action): F[ActionResult] = action match
    case Action.Sequence(actions)                   => F.pure(expand(actions.toList, sequential = true))
    case Action.FetchSecurities(exchanges)          => completed(security.fetchLatest(exchanges))
    case Action.FetchCompanyProfiles(filter, limit) =>
      security.findTickersBy(filter, limit).map(perTicker(_)(Action.UpdateCompanyProfiles(_)))
    case Action.FetchPriceAnalytics(filter, limit) =>
      profiles.findTickersBy(filter, limit).map(perTicker(_)(Action.UpdatePriceAnalytics(_)))
    case Action.FetchFinancialMetrics(filter, limit) =>
      profiles.findTickersBy(filter, limit).map(perTicker(_)(Action.UpdateFinancialMetrics(_)))
    case Action.UpdateCompanyProfiles(tickers) =>
      update(tickers)(Action.UpdateCompanyProfiles(_))(ticker => profiles.get(ticker, true).void)
    case Action.UpdatePriceAnalytics(tickers) =>
      update(tickers)(Action.UpdatePriceAnalytics(_))(ticker => prices.findPriceAnalytics(ticker, true).void)
    case Action.UpdateFinancialMetrics(tickers) =>
      update(tickers)(Action.UpdateFinancialMetrics(_))(ticker => financials.find(ticker, true).void)
    case Action.RefreshInvestmentRanking =>
      completed(rankings.refresh)
    case Action.CreateMonthlyScreen       => completed(screens.createMonthlyScreen.void)
    case Action.CaptureMonthlyEvaluations => completed(evaluations.captureMonthlyEvaluations.void)
    case Action.RefreshInvestmentResearch =>
      val actions = List(
        Action.Sequence(NonEmptyList.of(Action.RefreshInvestmentRanking, Action.CreateMonthlyScreen)),
        Action.CaptureMonthlyEvaluations
      )
      F.pure(expand(actions))
    case Action.RefreshEvaluationPrices(ticker, through) =>
      completed(prices.refreshHistory(ticker, through))
    case Action.RecordCompanyProfileUpdate(tickers)                  => completed(security.recordCompanyProfileUpdate(tickers))
    case Action.RecordPriceAnalyticsUpdate(tickers)                  => completed(profiles.recordPriceAnalyticsUpdate(tickers))
    case Action.RecordFinancialMetricsUpdate(tickers)                => completed(profiles.recordFinancialMetricsUpdate(tickers))
    case Action.SetSecurityActive(ticker, active, requestedAt)       => completed(security.setActive(ticker, active, requestedAt))
    case Action.SetCompanyProfileActive(ticker, active, requestedAt) => completed(profiles.setActive(ticker, active, requestedAt))

object ActionHandler:
  def make[F[_]: Monad](services: Services[F]): F[ActionHandler[F]] =
    Monad[F].pure(
      LiveActionHandler(
        services.security,
        services.companyProfile,
        services.price,
        services.financialMetrics,
        services.investmentScreens,
        services.investmentRankings,
        services.investmentEvaluations
      )
    )
