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
  def dataQuality: DataQualityService[F]

object Services:
  def make[F[_]: {Async, Clock, Logger}](
      clients: Clients[F],
      repos: Repositories[F],
      ad: ActionDispatcher[F],
      investmentConfig: InvestmentConfig
  ): F[Services[F]] =
    for
      s          <- SecurityService.make(repos.security, clients.marketData)
      cp         <- CompanyProfileService.make(repos.companyProfile, clients.marketData, repos.security)
      p          <- PriceService.make(repos.priceAnalytics, repos.latestPrice, clients.marketData, repos.companyProfile, repos.priceHistory)
      c          <- CommandService.make(repos.command, ad, repos.jobs)
      st         <- StockService.make(repos.stock, ad, repos.security, repos.companyProfile)
      fm         <- FinancialMetricsService.make(repos.financialMetrics, clients.marketData, repos.companyProfile, repos.financialHistory)
      investment <- InvestmentScreenService.make(
        repos.stock,
        repos.financialHistory,
        repos.investmentScreens,
        repos.investmentEvaluations,
        ProviderEvaluationPriceHistory.make(clients.marketData, repos.priceHistory, investmentConfig.totalReturnAudited),
        investmentConfig,
        InvestmentRecipe(
          maxSnapshotAgeDays = investmentConfig.snapshotFreshnessDays,
          maxAnnualAgeDays = investmentConfig.annualFreshnessDays,
          maxPriceAgeDays = investmentConfig.priceFreshnessDays,
          maxQuarterlyAgeDays = investmentConfig.quarterlyFreshnessDays
        ),
        repos.investmentRankings,
        repos.jobs
      )
      quality <- DataQualityService.make(repos, investmentConfig)
    yield new Services[F]:
      override def security: SecurityService[F]                  = s
      override def companyProfile: CompanyProfileService[F]      = cp
      override def price: PriceService[F]                        = p
      override def command: CommandService[F]                    = c
      override def stock: StockService[F]                        = st
      override def financialMetrics: FinancialMetricsService[F]  = fm
      override def investmentScreens: InvestmentScreenService[F] = investment
      override def dataQuality: DataQualityService[F]            = quality
