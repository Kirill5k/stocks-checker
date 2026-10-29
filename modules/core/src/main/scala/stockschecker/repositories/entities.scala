package stockschecker.repositories

import stockschecker.domain.DatasetStatus
import stockschecker.domain.ListingStatus
import stockschecker.domain.Provider
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import stockschecker.actions.Action
import stockschecker.domain.{
  Command,
  CommandId,
  CompanyProfile,
  CreateCommand,
  DataQualityReport,
  Dataset,
  DatasetAvailability,
  DateRange,
  EvaluationPriceSeries,
  Exchange,
  ExcludedSecurity,
  FinancialMetric,
  FinancialMetrics,
  FinancialObservation,
  FinancialSnapshot,
  FinancialSnapshotIssue,
  InvestmentCandidate,
  InvestmentCohort,
  InvestmentEvaluation,
  InvestmentRanking,
  InvestmentRankingId,
  InvestmentRankingSummary,
  InvestmentRecipe,
  InvestmentScorecard,
  InvestmentScreen,
  InvestmentScreenId,
  InvestmentScreenInput,
  InvestmentScreenRequest,
  InvestmentScreenSummary,
  Job,
  JobId,
  JobState,
  PriceAdjustment,
  PriceAnalytics,
  PriceAnalyticsBasis,
  PriceHistorySnapshot,
  PricePerformanceSummary,
  Schedule,
  Sector,
  Security,
  SecurityKind,
  Stock,
  StockAnalysisMetrics,
  StockAnalysisScores,
  Ticker
}
import stockschecker.domain.investment.EvaluationCapture

import java.time.{Instant, LocalDate, YearMonth}

private[repositories] object entities extends MongoJsonCodecs {

  final case class Entity(_id: Ticker) derives Codec.AsObject

  final case class SecurityEntity(
      _id: Ticker,
      ticker: Ticker,
      exchange: Exchange,
      name: String,
      kind: SecurityKind,
      isActive: Boolean,
      companyProfileLastUpdatedAt: Option[Instant],
      createdAt: Instant,
      updatedAt: Instant,
      listingStatus: ListingStatus,
      exclusionReason: Option[String] = None,
      activeChangedAt: Option[String] = None
  ) derives Codec.AsObject:
    def toDomain: Security =
      Security(
        ticker = _id,
        exchange = exchange,
        name = name,
        kind = kind,
        isActive = isActive,
        companyProfileLastUpdatedAt = companyProfileLastUpdatedAt,
        listingStatus = listingStatus,
        exclusionReason = exclusionReason
      )

  object SecurityEntity:
    given MongoCodecProvider[SecurityEntity] = deriveCirceCodecProvider[SecurityEntity]

  final case class CompanyProfileEntity(
      _id: Ticker,
      isActive: Boolean,
      name: String,
      country: String,
      industry: String,
      sector: Option[Sector],
      description: Option[String],
      website: String,
      ipoDate: Option[LocalDate],
      currency: String,
      marketCap: Long,
      priceAnalyticsLastUpdatedAt: Option[Instant],
      financialMetricsLastUpdatedAt: Option[Instant],
      createdAt: Instant,
      updatedAt: Instant,
      activeChangedAt: Option[String] = None
  ) derives Codec.AsObject:
    def toDomain: CompanyProfile =
      CompanyProfile(
        ticker = _id,
        name = name,
        country = country,
        industry = industry,
        sector = sector,
        description = description,
        website = website,
        ipoDate = ipoDate,
        currency = currency,
        marketCap = marketCap,
        priceAnalyticsLastUpdatedAt = priceAnalyticsLastUpdatedAt,
        financialMetricsLastUpdatedAt = financialMetricsLastUpdatedAt
      )

  object CompanyProfileEntity:
    given MongoCodecProvider[CompanyProfileEntity] = deriveCirceCodecProvider[CompanyProfileEntity]

  final case class PriceAnalyticsEntity(
      _id: Ticker,
      performanceSummary: PricePerformanceSummary,
      metrics: StockAnalysisMetrics,
      scores: StockAnalysisScores,
      createdAt: Instant,
      updatedAt: Instant,
      basis: Option[PriceAnalyticsBasis] = None
  ) derives Codec.AsObject:
    def toDomain: PriceAnalytics =
      PriceAnalytics(
        ticker = _id,
        performanceSummary = performanceSummary,
        metrics = metrics,
        scores = scores,
        basis = basis
      )

  object PriceAnalyticsEntity:
    given MongoCodecProvider[PriceAnalyticsEntity] = deriveCirceCodecProvider[PriceAnalyticsEntity]

  final case class FinancialMetricsEntity(
      _id: Ticker,
      peRatioTtm: Option[BigDecimal],
      epsTtm: Option[BigDecimal],
      roeTtm: Option[BigDecimal],
      dividendYieldAnnual: Option[BigDecimal],
      debtToEquityAnnual: Option[BigDecimal],
      profitMarginTtm: Option[BigDecimal],
      freeCashFlowPerShareTtm: Option[BigDecimal],
      revenueGrowth5Y: Option[BigDecimal],
      epsGrowth5Y: Option[BigDecimal],
      priceHigh52Week: Option[BigDecimal],
      priceLow52Week: Option[BigDecimal],
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: FinancialMetrics =
      FinancialMetrics(
        ticker = _id,
        peRatioTtm = peRatioTtm,
        epsTtm = epsTtm,
        roeTtm = roeTtm,
        dividendYieldAnnual = dividendYieldAnnual,
        debtToEquityAnnual = debtToEquityAnnual,
        profitMarginTtm = profitMarginTtm,
        freeCashFlowPerShareTtm = freeCashFlowPerShareTtm,
        revenueGrowth5Y = revenueGrowth5Y,
        epsGrowth5Y = epsGrowth5Y,
        priceHigh52Week = priceHigh52Week,
        priceLow52Week = priceLow52Week
      )

  object FinancialMetricsEntity:
    given MongoCodecProvider[FinancialMetricsEntity] = deriveCirceCodecProvider[FinancialMetricsEntity]

  final case class CommandEntity(
      _id: ObjectId,
      isActive: Boolean,
      action: Action,
      schedule: Schedule,
      lastExecutedAt: Option[Instant],
      executionCount: Int,
      maxExecutions: Option[Int],
      version: Long,
      nextRunAt: Option[Instant],
      activeRunId: Option[JobId]
  ) derives Codec.AsObject:
    def toDomain: Command =
      Command(
        id = CommandId(_id),
        isActive = isActive,
        action = action,
        schedule = schedule,
        lastExecutedAt = lastExecutedAt,
        executionCount = executionCount,
        maxExecutions = maxExecutions,
        version = version,
        nextRunAt = nextRunAt,
        activeRunId = activeRunId
      )

  object CommandEntity:
    given MongoCodecProvider[CommandEntity]     = deriveCirceCodecProvider[CommandEntity]
    def from(cmd: CreateCommand): CommandEntity =
      CommandEntity(
        _id = ObjectId.gen,
        isActive = true,
        action = cmd.action,
        schedule = cmd.schedule,
        lastExecutedAt = None,
        executionCount = 0,
        maxExecutions = cmd.maxExecutions,
        version = 0L,
        nextRunAt = None,
        activeRunId = None
      )

  // The API codec encodes Instants as JSON strings; Mongo claims and updates use BSON dates.
  private given Codec.AsObject[Job] = Codec.AsObject.derived[Job]

  final case class JobEntity(_id: JobId, job: Job) derives Codec.AsObject

  object JobEntity:
    given MongoCodecProvider[JobEntity] = deriveCirceCodecProvider[JobEntity]

  final case class JobStateCount(_id: JobState, count: Long) derives Codec.AsObject

  object JobStateCount:
    given MongoCodecProvider[JobStateCount] = deriveCirceCodecProvider[JobStateCount]

  final case class JobIdEntity(_id: JobId) derives Codec.AsObject

  object JobIdEntity:
    given MongoCodecProvider[JobIdEntity] = deriveCirceCodecProvider[JobIdEntity]

  final case class InvestmentScreenEntity(
      _id: InvestmentScreenId,
      createdAt: Instant,
      rankingId: InvestmentRankingId,
      request: InvestmentScreenRequest,
      cohorts: List[InvestmentCohort],
      summary: InvestmentScreenSummary
  ) derives Codec.AsObject:
    def toDomain: InvestmentScreen =
      InvestmentScreen(
        id = _id,
        createdAt = createdAt,
        rankingId = rankingId,
        request = request,
        cohorts = cohorts,
        summary = summary
      )

  object InvestmentScreenEntity:
    given MongoCodecProvider[InvestmentScreenEntity] = deriveCirceCodecProvider[InvestmentScreenEntity]

    def from(header: InvestmentScreen): InvestmentScreenEntity =
      InvestmentScreenEntity(
        _id = header.id,
        createdAt = header.createdAt,
        rankingId = header.rankingId,
        request = header.request,
        cohorts = header.cohorts,
        summary = header.summary
      )

  final case class InvestmentScreenWithRankingEntity(
      screen: InvestmentScreenEntity,
      rankings: List[InvestmentRankingEntity]
  ) derives Codec.AsObject

  object InvestmentScreenWithRankingEntity:
    given MongoCodecProvider[InvestmentScreenWithRankingEntity] = deriveCirceCodecProvider[InvestmentScreenWithRankingEntity]

  final case class InvestmentRankingEntity(
      _id: String,
      createdAt: Instant,
      recipe: InvestmentRecipe,
      summary: InvestmentRankingSummary
  ) derives Codec.AsObject:
    def toDomain: InvestmentRanking =
      InvestmentRanking(
        id = InvestmentRankingId(_id),
        createdAt = createdAt,
        recipe = recipe,
        summary = summary
      )

  object InvestmentRankingEntity:
    given MongoCodecProvider[InvestmentRankingEntity] = deriveCirceCodecProvider[InvestmentRankingEntity]

    def from(header: InvestmentRanking): InvestmentRankingEntity =
      InvestmentRankingEntity(
        _id = header.id.value,
        createdAt = header.createdAt,
        recipe = header.recipe,
        summary = header.summary
      )

  final case class InvestmentRankingIdEntity(_id: String) derives Codec.AsObject

  object InvestmentRankingIdEntity:
    given MongoCodecProvider[InvestmentRankingIdEntity] = deriveCirceCodecProvider[InvestmentRankingIdEntity]

  final case class InvestmentRankingInputEntity(
      _id: String,
      rankingId: InvestmentRankingId,
      ordinal: Int,
      input: InvestmentScreenInput,
      scorecard: InvestmentScorecard,
      createdAt: Instant
  ) derives Codec.AsObject

  object InvestmentRankingInputEntity:
    given MongoCodecProvider[InvestmentRankingInputEntity] = deriveCirceCodecProvider[InvestmentRankingInputEntity]

  final case class InvestmentEvaluationEntity(
      _id: String,
      screenId: InvestmentScreenId,
      month: YearMonth,
      evaluation: InvestmentEvaluation,
      complete: Boolean,
      settled: Boolean,
      finished: Boolean,
      requestedHistory: List[Ticker]
  ) derives Codec.AsObject

  object InvestmentEvaluationEntity:
    given MongoCodecProvider[InvestmentEvaluationEntity] = deriveCirceCodecProvider[InvestmentEvaluationEntity]

  final case class InvestmentEvaluationContextEntity(
      screen: InvestmentScreenEntity,
      monthly: List[InvestmentEvaluation],
      finished: List[InvestmentEvaluation]
  ) derives Codec.AsObject:
    def toDomain: EvaluationCapture.Context =
      EvaluationCapture.Context(screen.toDomain, monthly.headOption, finished.headOption)

  object InvestmentEvaluationContextEntity:
    given MongoCodecProvider[InvestmentEvaluationContextEntity] = deriveCirceCodecProvider[InvestmentEvaluationContextEntity]

  final case class InvestmentEvaluationStateEntity(requestedHistory: List[Ticker]) derives Codec.AsObject:
    def toDomain: EvaluationCapture.State = EvaluationCapture.State(requestedHistory.toSet)

  final case class InvestmentEvaluationTaskEntity(
      screen: InvestmentScreenEntity,
      month: YearMonth,
      existing: List[InvestmentEvaluationStateEntity]
  ) derives Codec.AsObject:
    def toDomain: EvaluationCapture.Task = EvaluationCapture.Task(screen.toDomain, month, existing.headOption.map(_.toDomain))

  object InvestmentEvaluationTaskEntity:
    given MongoCodecProvider[InvestmentEvaluationTaskEntity] = deriveCirceCodecProvider[InvestmentEvaluationTaskEntity]

  final case class DatasetAvailabilityEntity(
      _id: String,
      ticker: Ticker,
      provider: Provider,
      dataset: Dataset,
      status: DatasetStatus,
      lastAttemptAt: Instant,
      lastSuccessAt: Option[Instant],
      failure: Option[String]
  ) derives Codec.AsObject:
    def toDomain: DatasetAvailability =
      DatasetAvailability(ticker, provider, dataset, status, lastAttemptAt, lastSuccessAt, failure)

  object DatasetAvailabilityEntity:
    given MongoCodecProvider[DatasetAvailabilityEntity] = deriveCirceCodecProvider[DatasetAvailabilityEntity]

  final case class DatasetStatusCount(
      _id: DatasetStatus,
      count: Int
  ) derives Codec.AsObject

  final case class DataQualityReportEntity(
      datasets: List[DatasetAvailabilityEntity],
      exclusions: List[ExcludedSecurity],
      countsByStatus: List[DatasetStatusCount],
      rankings: List[InvestmentRankingEntity]
  ) derives Codec.AsObject:
    def toDomain(at: Instant): DataQualityReport =
      val ranking  = rankings.headOption
      val coverage = ranking
        .map { header =>
          header.summary.excludedByReason ++ Map("eligible" -> header.summary.eligibleCount, "universe" -> header.summary.universeCount)
        }
        .getOrElse(Map.empty)
      DataQualityReport(
        at,
        datasets.map(_.toDomain),
        exclusions,
        countsByStatus.map(count => count._id -> count.count).toMap,
        coverage,
        ranking.map(_.createdAt)
      )

  object DataQualityReportEntity:
    given MongoCodecProvider[DataQualityReportEntity] = deriveCirceCodecProvider[DataQualityReportEntity]

  private given Codec.AsObject[PriceHistorySnapshot] = Codec.AsObject.derived[PriceHistorySnapshot]

  final case class PriceHistoryEntity(
      _id: String,
      ticker: Ticker,
      adjustment: PriceAdjustment,
      data: PriceHistorySnapshot
  ) derives Codec.AsObject

  object PriceHistoryEntity:
    given MongoCodecProvider[PriceHistoryEntity] = deriveCirceCodecProvider[PriceHistoryEntity]

  final case class EvaluationPriceSeriesEntity(data: PriceHistorySnapshot, candleCount: Int) derives Codec.AsObject:
    def toDomain(range: DateRange): EvaluationPriceSeries = EvaluationPriceSeries.from(data, range, candleCount)

  object EvaluationPriceSeriesEntity:
    given MongoCodecProvider[EvaluationPriceSeriesEntity] = deriveCirceCodecProvider[EvaluationPriceSeriesEntity]

  final case class ProviderBudget(
      day: LocalDate,
      used: Long,
      nextAllowed: Instant,
      version: Long,
      cooldownUntil: Option[Instant] = None
  ) derives Codec.AsObject

  object ProviderBudget:
    given MongoCodecProvider[ProviderBudget] = deriveCirceCodecProvider[ProviderBudget]

  final case class ProviderBudgetEntity(
      _id: String,
      budget: ProviderBudget
  ) derives Codec.AsObject

  object ProviderBudgetEntity:
    given MongoCodecProvider[ProviderBudgetEntity] = deriveCirceCodecProvider[ProviderBudgetEntity]

  final case class FinancialSnapshotEntity(
      _id: Ticker,
      ticker: Ticker,
      provider: Provider,
      observedAt: Instant,
      metrics: Map[FinancialMetric, BigDecimal],
      annual: List[FinancialObservation],
      quarterly: List[FinancialObservation],
      payloadHash: String,
      mappingVersion: String,
      issues: List[FinancialSnapshotIssue]
  ) derives Codec.AsObject:
    def toDomain: FinancialSnapshot =
      FinancialSnapshot(ticker, provider, observedAt, metrics, annual, quarterly, payloadHash, mappingVersion, issues)

  object FinancialSnapshotEntity:
    given MongoCodecProvider[FinancialSnapshotEntity] = deriveCirceCodecProvider[FinancialSnapshotEntity]

    def from(snapshot: FinancialSnapshot): FinancialSnapshotEntity =
      FinancialSnapshotEntity(
        snapshot.ticker,
        snapshot.ticker,
        snapshot.provider,
        snapshot.observedAt,
        snapshot.metrics,
        snapshot.annual,
        snapshot.quarterly,
        snapshot.payloadHash,
        snapshot.mappingVersion,
        snapshot.issues
      )

  final case class StockEntity(
      security: SecurityEntity,
      profile: List[CompanyProfileEntity],
      priceAnalytics: List[PriceAnalyticsEntity],
      financialMetrics: List[FinancialMetricsEntity]
  ) derives Codec.AsObject:
    def toDomain: Stock =
      Stock(
        security = security.toDomain,
        profile = profile.headOption.map(_.toDomain),
        priceAnalytics = priceAnalytics.headOption.map(_.toDomain),
        financialMetrics = financialMetrics.headOption.map(_.toDomain)
      )

  final case class InvestmentCandidateEntity(
      stock: StockEntity,
      financialSnapshots: List[FinancialSnapshotEntity]
  ) derives Codec.AsObject:
    def toDomain: InvestmentCandidate =
      InvestmentCandidate(stock.toDomain, financialSnapshots.headOption.map(_.toDomain))

  object InvestmentCandidateEntity:
    given MongoCodecProvider[InvestmentCandidateEntity] = deriveCirceCodecProvider[InvestmentCandidateEntity]
}
