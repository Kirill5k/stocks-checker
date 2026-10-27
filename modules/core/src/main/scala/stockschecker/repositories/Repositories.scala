package stockschecker.repositories

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.kernel.Async
import kirill5k.common.cats.Clock
import mongo4cats.database.MongoDatabase

trait Repositories[F[_]]:
  def security: SecurityRepository[F]
  def companyProfile: CompanyProfileRepository[F]
  def latestPrice: LatestPriceRepository[F]
  def command: CommandRepository[F]
  def stock: StockRepository[F]
  def priceAnalytics: PriceAnalyticsRepository[F]
  def financialMetrics: FinancialMetricsRepository[F]
  def financialHistory: FinancialHistoryRepository[F]
  def priceHistory: PriceHistoryRepository[F]
  def dataQuality: DataQualityRepository[F]
  def providerBudgets: ProviderBudgetRepository[F]
  def jobs: JobRepository[F]
  def investmentScreens: InvestmentScreenRepository[F]
  def investmentEvaluations: InvestmentEvaluationRepository[F]
  def investmentRankings: InvestmentRankingRepository[F]
  def investmentRankingInputs: InvestmentRankingInputRepository[F]
  def evaluationCapturePlans: EvaluationCapturePlanRepository[F]

object Repositories:
  def make[F[_]: Clock](db: MongoDatabase[F])(using F: Async[F]): F[Repositories[F]] =
    for
      s             <- SecurityRepository.make(db)
      cp            <- CompanyProfileRepository.make(db)
      lp            <- LatestPriceRepository.make(db)
      c             <- CommandRepository.make(db)
      st            <- StockRepository.make(db)
      pa            <- PriceAnalyticsRepository.make(db)
      fm            <- FinancialMetricsRepository.make(db)
      fh            <- FinancialHistoryRepository.make(db)
      ph            <- PriceHistoryRepository.make(db)
      dq            <- DataQualityRepository.make(db)
      budgets       <- ProviderBudgetRepository.make[F](db)
      jobRepo       <- JobRepository.make(db)
      screens       <- InvestmentScreenRepository.make(db)
      evaluations   <- InvestmentEvaluationRepository.make(db)
      rankings      <- InvestmentRankingRepository.make(db)
      rankingInputs <- InvestmentRankingInputRepository.make(db)
      capturePlans  <- EvaluationCapturePlanRepository.make(db)
    yield new Repositories[F]:
      override def security: SecurityRepository[F]                              = s
      override def companyProfile: CompanyProfileRepository[F]                  = cp
      override def latestPrice: LatestPriceRepository[F]                        = lp
      override def command: CommandRepository[F]                                = c
      override def stock: StockRepository[F]                                    = st
      override def priceAnalytics: PriceAnalyticsRepository[F]                  = pa
      override def financialMetrics: FinancialMetricsRepository[F]              = fm
      override def financialHistory: FinancialHistoryRepository[F]              = fh
      override def priceHistory: PriceHistoryRepository[F]                      = ph
      override def dataQuality: DataQualityRepository[F]                        = dq
      override def providerBudgets: ProviderBudgetRepository[F]                 = budgets
      override def jobs: JobRepository[F]                                       = jobRepo
      override def investmentScreens: InvestmentScreenRepository[F]             = screens
      override def investmentEvaluations: InvestmentEvaluationRepository[F]     = evaluations
      override def investmentRankings: InvestmentRankingRepository[F]           = rankings
      override def investmentRankingInputs: InvestmentRankingInputRepository[F] = rankingInputs
      override def evaluationCapturePlans: EvaluationCapturePlanRepository[F]   = capturePlans
