package stockschecker.actions

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.domain.{DateRange, JobPlan}
import stockschecker.services.{
  CompanyProfileService,
  FinancialMetricsService,
  InvestmentEvaluationService,
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
    evaluations: InvestmentEvaluationService[F]
)(using F: Monad[F])
    extends ActionHandler[F]:
  private def completed(effect: F[Unit]): F[ActionResult]                              = effect.as(ActionResult.Completed)
  private def expand(actions: List[Action], sequential: Boolean = false): ActionResult =
    ActionResult.Expand(JobPlan(actions, sequential))

  override def execute(action: Action): F[ActionResult] = action match
    case Action.LegacyRetired              => F.pure(ActionResult.Cancelled("Legacy scheduling/audit action retired during upgrade"))
    case Action.Sequence(actions)          => F.pure(expand(actions.toList, sequential = true))
    case Action.FetchSecurities(exchanges) => completed(security.fetchLatest(exchanges))
    case Action.FetchCompanyProfiles(filter, limit) =>
      security.findTickersBy(filter, limit).map(tickers => expand(tickers.map(t => Action.UpdateCompanyProfiles(NonEmptyList.one(t)))))
    case Action.FetchPriceAnalytics(filter, limit) =>
      profiles.findTickersBy(filter, limit).map(tickers => expand(tickers.map(t => Action.UpdatePriceAnalytics(NonEmptyList.one(t)))))
    case Action.FetchFinancialMetrics(filter, limit) =>
      profiles.findTickersBy(filter, limit).map(tickers => expand(tickers.map(t => Action.UpdateFinancialMetrics(NonEmptyList.one(t)))))
    case Action.UpdateCompanyProfiles(tickers) if tickers.size == 1  => completed(profiles.get(tickers.head, true).void)
    case Action.UpdatePriceAnalytics(tickers) if tickers.size == 1   => completed(prices.findPriceAnalytics(tickers.head, true).void)
    case Action.UpdateFinancialMetrics(tickers) if tickers.size == 1 => completed(financials.find(tickers.head, true).void)
    case Action.UpdateCompanyProfiles(tickers) => F.pure(expand(tickers.toList.map(t => Action.UpdateCompanyProfiles(NonEmptyList.one(t)))))
    case Action.UpdatePriceAnalytics(tickers)  => F.pure(expand(tickers.toList.map(t => Action.UpdatePriceAnalytics(NonEmptyList.one(t)))))
    case Action.UpdateFinancialMetrics(tickers) =>
      F.pure(expand(tickers.toList.map(t => Action.UpdateFinancialMetrics(NonEmptyList.one(t)))))
    case Action.RefreshInvestmentResearch => completed(screens.generateScheduledScreen >> evaluations.captureMonthlyEvaluations.void)
    case Action.RefreshEvaluationPrices(ticker, from, through) => completed(evaluations.refreshHistory(ticker, DateRange(from, through)))
    case Action.DeactivateSecurity(ticker)                     => completed(security.deactivate(ticker))
    case Action.DeactivateCompanyProfile(ticker)               => completed(profiles.deactivate(ticker))

object ActionHandler:
  def make[F[_]: Monad](services: Services[F]): F[ActionHandler[F]] =
    Monad[F].pure(
      LiveActionHandler(
        services.security,
        services.companyProfile,
        services.price,
        services.financialMetrics,
        services.investmentScreens,
        services.investmentEvaluations
      )
    )
