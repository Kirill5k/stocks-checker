package stockschecker.repositories

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
  EvaluationCapturePlan,
  Exchange,
  FinancialMetrics,
  InvestmentCandidate,
  InvestmentCohort,
  InvestmentEvaluation,
  InvestmentRankingHeader,
  InvestmentRankingId,
  InvestmentRankingMetadata,
  InvestmentRankingSummary,
  InvestmentRecipe,
  InvestmentScorecard,
  InvestmentScreenHeader,
  InvestmentScreenId,
  InvestmentScreenRequest,
  InvestmentScreenSummary,
  Job,
  JobId,
  JobState,
  LatestPrice,
  PriceAnalytics,
  PriceAnalyticsBasis,
  PricePerformanceSummary,
  Schedule,
  Security,
  SecurityKind,
  Stock,
  StockAnalysisMetrics,
  StockAnalysisScores,
  Ticker
}

import java.time.{Instant, LocalDate}

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
      listingStatus: Option[String] = None,
      exclusionReason: Option[String] = None
  ) derives Codec.AsObject:
    def toDomain: Security =
      Security(
        ticker = _id,
        exchange = exchange,
        name = name,
        kind = kind,
        isActive = isActive,
        companyProfileLastUpdatedAt = companyProfileLastUpdatedAt,
        listingStatus = listingStatus.getOrElse("listed"),
        exclusionReason = exclusionReason
      )

  object SecurityEntity:
    given MongoCodecProvider[SecurityEntity] = deriveCirceCodecProvider[SecurityEntity]

  final case class CompanyProfileEntity(
      _id: Ticker,
      isActive: Option[Boolean],
      name: String,
      country: String,
      industry: String,
      description: Option[String],
      website: String,
      ipoDate: Option[LocalDate],
      currency: String,
      marketCap: Long,
      priceAnalyticsLastUpdatedAt: Option[Instant],
      financialMetricsLastUpdatedAt: Option[Instant],
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: CompanyProfile =
      CompanyProfile(
        ticker = _id,
        name = name,
        country = country,
        industry = industry,
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

  final case class LatestPriceEntity(
      _id: String,
      ticker: Ticker,
      price: BigDecimal,
      date: LocalDate,
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: LatestPrice =
      LatestPrice(
        ticker = ticker,
        price = price,
        date = date
      )

  object LatestPriceEntity:
    given MongoCodecProvider[LatestPriceEntity] = deriveCirceCodecProvider[LatestPriceEntity]

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
      maxExecutions: Option[Int]
  ) derives Codec.AsObject:
    def toDomain: Command =
      Command(
        id = CommandId(_id),
        isActive = isActive,
        action = action,
        schedule = schedule,
        lastExecutedAt = lastExecutedAt,
        executionCount = executionCount,
        maxExecutions = maxExecutions
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
        maxExecutions = cmd.maxExecutions
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
    def toDomain: InvestmentScreenHeader =
      InvestmentScreenHeader(
        id = _id,
        createdAt = createdAt,
        rankingId = rankingId,
        request = request,
        cohorts = cohorts,
        summary = summary
      )

  object InvestmentScreenEntity:
    given MongoCodecProvider[InvestmentScreenEntity] = deriveCirceCodecProvider[InvestmentScreenEntity]

    def from(header: InvestmentScreenHeader): InvestmentScreenEntity =
      InvestmentScreenEntity(
        _id = header.id,
        createdAt = header.createdAt,
        rankingId = header.rankingId,
        request = header.request,
        cohorts = header.cohorts,
        summary = header.summary
      )

  final case class InvestmentRankingEntity(
      _id: String,
      createdAt: Instant,
      recipe: InvestmentRecipe,
      summary: Option[InvestmentRankingSummary]
  ) derives Codec.AsObject:
    def toDomain: InvestmentRankingMetadata =
      InvestmentRankingMetadata(
        id = InvestmentRankingId(_id),
        createdAt = createdAt,
        recipe = recipe,
        summary = summary
      )

  object InvestmentRankingEntity:
    given MongoCodecProvider[InvestmentRankingEntity] = deriveCirceCodecProvider[InvestmentRankingEntity]

    def from(header: InvestmentRankingHeader): InvestmentRankingEntity =
      InvestmentRankingEntity(
        _id = header.id.value,
        createdAt = header.createdAt,
        recipe = header.recipe,
        summary = Some(header.summary)
      )

  final case class InvestmentRankingOutcome(eligible: Boolean, exclusions: List[String]) derives Codec.AsObject

  object InvestmentRankingOutcome:
    given MongoCodecProvider[InvestmentRankingOutcome] = deriveCirceCodecProvider[InvestmentRankingOutcome]

  final case class InvestmentRankingReference(rankingId: InvestmentRankingId) derives Codec.AsObject

  object InvestmentRankingReference:
    given MongoCodecProvider[InvestmentRankingReference] = deriveCirceCodecProvider[InvestmentRankingReference]

  final case class InvestmentRankingIdEntity(_id: String) derives Codec.AsObject

  object InvestmentRankingIdEntity:
    given MongoCodecProvider[InvestmentRankingIdEntity] = deriveCirceCodecProvider[InvestmentRankingIdEntity]

  final case class InvestmentRankingInputEntity(
      _id: String,
      rankingId: InvestmentRankingId,
      ordinal: Int,
      inputOrdinal: Int,
      candidate: InvestmentCandidate,
      scorecard: InvestmentScorecard,
      createdAt: Option[Instant] = None
  ) derives Codec.AsObject

  object InvestmentRankingInputEntity:
    given MongoCodecProvider[InvestmentRankingInputEntity] = deriveCirceCodecProvider[InvestmentRankingInputEntity]

  final case class InvestmentEvaluationEntity(
      _id: String,
      screenId: InvestmentScreenId,
      month: String,
      evaluation: InvestmentEvaluation,
      complete: Option[Boolean] = None,
      settled: Option[Boolean] = None,
      finished: Option[Boolean] = None
  ) derives Codec.AsObject

  object InvestmentEvaluationEntity:
    given MongoCodecProvider[InvestmentEvaluationEntity] = deriveCirceCodecProvider[InvestmentEvaluationEntity]

  final case class InvestmentEvaluationScreenReference(screenId: InvestmentScreenId) derives Codec.AsObject

  object InvestmentEvaluationScreenReference:
    given MongoCodecProvider[InvestmentEvaluationScreenReference] = deriveCirceCodecProvider[InvestmentEvaluationScreenReference]

  final case class InvestmentEvaluationPlanEntity(_id: String, plan: EvaluationCapturePlan) derives Codec.AsObject

  object InvestmentEvaluationPlanEntity:
    given MongoCodecProvider[InvestmentEvaluationPlanEntity] = deriveCirceCodecProvider[InvestmentEvaluationPlanEntity]

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
}
