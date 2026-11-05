package stockschecker.services

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.actions.ActionDispatcher
import stockschecker.clients.Clients
import stockschecker.repositories.Repositories
import stockschecker.common.config.{InvestmentConfig, SecurityConfig}

trait Services[F[_]]:
  def security: SecurityService[F]
  def companyProfile: CompanyProfileService[F]
  def price: PriceService[F]
  def command: CommandService[F]
  def jobs: JobService[F]
  def stock: StockService[F]
  def financialMetrics: FinancialMetricsService[F]
  def investmentRankings: InvestmentRankingService[F]
  def investmentScreens: InvestmentScreenService[F]
  def investmentEvaluations: InvestmentEvaluationService[F]
  def dataQuality: DataQualityService[F]

object Services:
  def make[F[_]: {Async, Clock, Logger}](
      clients: Clients[F],
      repos: Repositories[F],
      ad: ActionDispatcher[F],
      investmentConfig: InvestmentConfig,
      securityConfig: SecurityConfig
  ): F[Services[F]] =
    val recipe = investmentConfig.recipe
    for
      st          <- StockService.make(repos.stock, ad)
      rankings    <- InvestmentRankingService.make(repos.investmentRankings, recipe)
      s           <- SecurityService.make(repos.security, clients.marketData, ad, securityConfig)
      quality     <- DataQualityService.make(repos.dataQuality, recipe)
      cp          <- CompanyProfileService.make(repos.companyProfile, clients.marketData, ad)
      p           <- PriceService.make(repos.priceAnalytics, ad, repos.priceHistory, clients.marketData)
      c           <- CommandService.make(repos.command, ad)
      j           <- JobService.make(repos.jobs)
      fm          <- FinancialMetricsService.make(repos.financialMetrics, clients.marketData, ad, repos.financialHistory)
      investment  <- InvestmentScreenService.make(repos.investmentScreens, ad, recipe)
      evaluations <- InvestmentEvaluationService.make(repos.investmentEvaluations, ad)
    yield new Services[F]:
      override def security: SecurityService[F]                          = s
      override def companyProfile: CompanyProfileService[F]              = cp
      override def price: PriceService[F]                                = p
      override def command: CommandService[F]                            = c
      override def jobs: JobService[F]                                   = j
      override def stock: StockService[F]                                = st
      override def financialMetrics: FinancialMetricsService[F]          = fm
      override def investmentRankings: InvestmentRankingService[F]       = rankings
      override def investmentScreens: InvestmentScreenService[F]         = investment
      override def investmentEvaluations: InvestmentEvaluationService[F] = evaluations
      override def dataQuality: DataQualityService[F]                    = quality
