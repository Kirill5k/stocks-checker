package stockschecker.services

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.actions.ActionDispatcher
import stockschecker.clients.Clients
import stockschecker.repositories.Repositories
import stockschecker.common.config.InvestmentConfig
import stockschecker.domain.InvestmentRecipe

trait Services[F[_]]:
  def security: SecurityService[F]
  def companyProfile: CompanyProfileService[F]
  def price: PriceService[F]
  def command: CommandService[F]
  def stock: StockService[F]
  def financialMetrics: FinancialMetricsService[F]
  def investmentScreens: InvestmentScreenService[F]
  def investmentEvaluations: InvestmentEvaluationService[F]
  def dataQuality: DataQualityService[F]

object Services:
  def make[F[_]: {Async, Clock, Logger}](
      clients: Clients[F],
      repos: Repositories[F],
      ad: ActionDispatcher[F],
      investmentConfig: InvestmentConfig
  ): F[Services[F]] =
    val recipe = InvestmentRecipe(
      maxSnapshotAgeDays = investmentConfig.snapshotFreshnessDays,
      maxAnnualAgeDays = investmentConfig.annualFreshnessDays,
      maxPriceAgeDays = investmentConfig.priceFreshnessDays,
      maxQuarterlyAgeDays = investmentConfig.quarterlyFreshnessDays
    )
    for
      _           <- Async[F].fromEither(recipe.validate.left.map(stockschecker.domain.errors.AppError.FailedValidation(_)))
      st          <- StockService.make(repos.stock, ad)
      rankings    <- InvestmentRankingService.make(st, repos.investmentRankings, recipe)
      s           <- SecurityService.make(repos.security, clients.marketData, ad)
      quality     <- DataQualityService.make(repos.dataQuality)
      cp          <- CompanyProfileService.make(repos.companyProfile, clients.marketData, ad)
      history     <- PriceHistoryService.make(clients.marketData, repos.priceHistory)
      p           <- PriceService.make(repos.priceAnalytics, repos.latestPrice, ad, history)
      c           <- CommandService.make(repos.command, ad)
      fm          <- FinancialMetricsService.make(repos.financialMetrics, clients.marketData, ad, repos.financialHistory)
      investment  <- InvestmentScreenService.make(repos.investmentScreens, rankings, recipe)
      evaluations <- InvestmentEvaluationService.make(investment, repos.investmentEvaluations, history, ad)
    yield new Services[F]:
      override def security: SecurityService[F]                          = s
      override def companyProfile: CompanyProfileService[F]              = cp
      override def price: PriceService[F]                                = p
      override def command: CommandService[F]                            = c
      override def stock: StockService[F]                                = st
      override def financialMetrics: FinancialMetricsService[F]          = fm
      override def investmentScreens: InvestmentScreenService[F]         = investment
      override def investmentEvaluations: InvestmentEvaluationService[F] = evaluations
      override def dataQuality: DataQualityService[F]                    = quality
