package stockschecker.controllers

import cats.effect.kernel.Async
import io.circe.Codec as CirceCodec
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.controllers.StockController.{StockQueryParams, StockView}
import stockschecker.domain.{Exchange, SecurityKind, Stock, StockAnalysisMetrics, StockAnalysisScores, StockFilters, StockSortField, Ticker, TimePeriod}
import stockschecker.services.StockService
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.{Instant, LocalDate}

final private class StockController[F[_]: Async](
    private val stockService: StockService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {

  private val getStockByTicker = secured(StockController.getStockByTickerEndpoint)
    .serverLogic { _ => ticker =>
      stockService.findByTicker(ticker).mapToResponse(StockView.from)
    }

  private val getAllStocks = secured(StockController.getAllStocksEndpoint)
    .serverLogic { _ => (params: StockQueryParams) =>
      val filters = StockFilters(
        exchange            = params.basic.exchange,
        kind                = params.basic.kind,
        isActive            = params.basic.isActive,
        country             = params.basic.country,
        minMarketCap        = params.market.minMarketCap,
        maxMarketCap        = params.market.maxMarketCap,
        minPrice            = params.market.minPrice,
        maxPrice            = params.market.maxPrice,
        minChange           = params.market.minChange,
        maxChange           = params.market.maxChange,
        period              = params.market.period,
        minOverallScore     = params.scores.minOverallScore,
        minCagrScore        = params.scores.minCagrScore,
        minVolatilityScore  = params.scores.minVolatilityScore,
        maxDrawdown         = params.scores.maxDrawdown,
        minConsistencyScore = params.scores.minConsistencyScore,
        minPE               = params.financial.minPE,
        maxPE               = params.financial.maxPE,
        minROE              = params.financial.minROE,
        maxDebtToEquity     = params.financial.maxDebtToEquity,
        minProfitMargin     = params.financial.minProfitMargin,
        maxProfitMargin     = params.financial.maxProfitMargin,
        minFreeCashFlow     = params.financial.minFreeCashFlow,
        minDividendYield    = params.financial.minDividendYield,
        maxDividendYield    = params.financial.maxDividendYield,
        minRevenueGrowth5Y  = params.financial.minRevenueGrowth5Y,
        minEpsGrowth5Y      = params.financial.minEpsGrowth5Y,
        sortBy              = params.basic.sortBy
      )
      stockService.findAll(filters, params.basic.limit).mapToResponse(_.map(StockView.from))
    }

  private val deactivateStock = secured(StockController.deactivateStockEndpoint)
    .serverLogic { _ => ticker =>
      stockService.deactivate(ticker).asResponse
    }

  override val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(List(getStockByTicker, getAllStocks, deactivateStock))
}

object StockController extends TapirJsonCirce with SchemaDerivation {

  // --- Query parameter groups (split to work around Tapir's 22-param limit) ---

  final case class BasicFilters(
      limit: Option[Int],
      exchange: Option[Exchange],
      kind: Option[SecurityKind],
      isActive: Option[Boolean],
      country: Option[String],
      sortBy: Option[StockSortField]
  )

  final case class MarketFilters(
      minMarketCap: Option[Long],
      maxMarketCap: Option[Long],
      minPrice: Option[BigDecimal],
      maxPrice: Option[BigDecimal],
      minChange: Option[BigDecimal],
      maxChange: Option[BigDecimal],
      period: Option[TimePeriod]
  )

  final case class ScoreFilters(
      minOverallScore: Option[BigDecimal],
      minCagrScore: Option[BigDecimal],
      minVolatilityScore: Option[BigDecimal],
      maxDrawdown: Option[BigDecimal],
      minConsistencyScore: Option[BigDecimal]
  )

  final case class FinancialFilters(
      minPE: Option[BigDecimal],
      maxPE: Option[BigDecimal],
      minROE: Option[BigDecimal],
      maxDebtToEquity: Option[BigDecimal],
      minProfitMargin: Option[BigDecimal],
      maxProfitMargin: Option[BigDecimal],
      minFreeCashFlow: Option[BigDecimal],
      minDividendYield: Option[BigDecimal],
      maxDividendYield: Option[BigDecimal],
      minRevenueGrowth5Y: Option[BigDecimal],
      minEpsGrowth5Y: Option[BigDecimal]
  )

  final case class StockQueryParams(
      basic: BasicFilters,
      market: MarketFilters,
      scores: ScoreFilters,
      financial: FinancialFilters
  )

  // --- Response views ---

  final private case class FinancialMetricsView(
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
      lastUpdatedAt: Option[Instant]
  ) derives CirceCodec.AsObject

  final private case class StockView(
      ticker: Ticker,
      name: String,
      security: StockSecurityView,
      profile: Option[StockCompanyProfileView],
      priceAnalytics: Option[PriceAnalyticsView],
      financialMetrics: Option[FinancialMetricsView]
  ) derives CirceCodec.AsObject

  private object StockView:
    def from(stock: Stock): StockView = StockView(
      ticker = stock.security.ticker,
      name = stock.security.name,
      security = StockSecurityView(
        exchange = stock.security.exchange,
        kind = stock.security.kind,
        isActive = stock.security.isActive
      ),
      profile = stock.profile.map(p =>
        StockCompanyProfileView(
          country = p.country,
          industry = p.industry,
          description = p.description,
          website = p.website,
          ipoDate = p.ipoDate,
          currency = p.currency,
          marketCap = p.marketCap,
          lastUpdatedAt = stock.security.companyProfileLastUpdatedAt
        )
      ),
      priceAnalytics = stock.priceAnalytics.map(pa =>
        PriceAnalyticsView(
          performanceSummary = StockPricePerformanceSummaryView(
            latestPrice = pa.performanceSummary.latestPrice,
            latestPriceDate = pa.performanceSummary.latestPriceDate,
            oneMonthChange = pa.performanceSummary.oneMonthChange,
            threeMonthChange = pa.performanceSummary.threeMonthChange,
            sixMonthChange = pa.performanceSummary.sixMonthChange,
            oneYearChange = pa.performanceSummary.oneYearChange,
            threeYearChange = pa.performanceSummary.threeYearChange,
            fiveYearChange = pa.performanceSummary.fiveYearChange,
            tenYearChange = pa.performanceSummary.tenYearChange,
            maxChange = pa.performanceSummary.maxChange
          ),
          metrics = pa.metrics,
          scores = pa.scores,
          lastUpdatedAt = stock.profile.flatMap(_.priceAnalyticsLastUpdatedAt)
        )
      ),
      financialMetrics = stock.financialMetrics.map(fm =>
        FinancialMetricsView(
          peRatioTtm = fm.peRatioTtm,
          epsTtm = fm.epsTtm,
          roeTtm = fm.roeTtm,
          dividendYieldAnnual = fm.dividendYieldAnnual,
          debtToEquityAnnual = fm.debtToEquityAnnual,
          profitMarginTtm = fm.profitMarginTtm,
          freeCashFlowPerShareTtm = fm.freeCashFlowPerShareTtm,
          revenueGrowth5Y = fm.revenueGrowth5Y,
          epsGrowth5Y = fm.epsGrowth5Y,
          priceHigh52Week = fm.priceHigh52Week,
          priceLow52Week = fm.priceLow52Week,
          lastUpdatedAt = stock.profile.flatMap(_.financialMetricsLastUpdatedAt)
        )
      )
    )

  final private case class StockSecurityView(
      exchange: Exchange,
      kind: SecurityKind,
      isActive: Boolean
  ) derives CirceCodec.AsObject

  final private case class StockCompanyProfileView(
      country: String,
      industry: String,
      description: Option[String],
      website: String,
      ipoDate: Option[LocalDate],
      currency: String,
      marketCap: Long,
      lastUpdatedAt: Option[Instant]
  ) derives CirceCodec.AsObject

  final private case class StockPricePerformanceSummaryView(
      latestPrice: BigDecimal,
      latestPriceDate: LocalDate,
      oneMonthChange: Option[BigDecimal],
      threeMonthChange: Option[BigDecimal],
      sixMonthChange: Option[BigDecimal],
      oneYearChange: Option[BigDecimal],
      threeYearChange: Option[BigDecimal],
      fiveYearChange: Option[BigDecimal],
      tenYearChange: Option[BigDecimal],
      maxChange: Option[BigDecimal]
  ) derives CirceCodec.AsObject

  final private case class PriceAnalyticsView(
      performanceSummary: StockPricePerformanceSummaryView,
      metrics: StockAnalysisMetrics,
      scores: StockAnalysisScores,
      lastUpdatedAt: Option[Instant]
  ) derives CirceCodec.AsObject

  private val stockByTickerPath = "stocks" / path[Ticker]("ticker")

  private val basicInput =
    query[Option[Int]]("limit")
      .and(query[Option[Exchange]]("exchange"))
      .and(query[Option[SecurityKind]]("kind"))
      .and(query[Option[Boolean]]("isActive"))
      .and(query[Option[String]]("country"))
      .and(query[Option[StockSortField]]("sortBy"))
      .mapTo[BasicFilters]

  private val marketInput =
    query[Option[Long]]("minMarketCap")
      .and(query[Option[Long]]("maxMarketCap"))
      .and(query[Option[BigDecimal]]("minPrice"))
      .and(query[Option[BigDecimal]]("maxPrice"))
      .and(query[Option[BigDecimal]]("minChange"))
      .and(query[Option[BigDecimal]]("maxChange"))
      .and(query[Option[TimePeriod]]("period"))
      .mapTo[MarketFilters]

  private val scoreInput =
    query[Option[BigDecimal]]("minOverallScore")
      .and(query[Option[BigDecimal]]("minCagrScore"))
      .and(query[Option[BigDecimal]]("minVolatilityScore"))
      .and(query[Option[BigDecimal]]("maxDrawdown"))
      .and(query[Option[BigDecimal]]("minConsistencyScore"))
      .mapTo[ScoreFilters]

  private val financialInput =
    query[Option[BigDecimal]]("minPE")
      .and(query[Option[BigDecimal]]("maxPE"))
      .and(query[Option[BigDecimal]]("minROE"))
      .and(query[Option[BigDecimal]]("maxDebtToEquity"))
      .and(query[Option[BigDecimal]]("minProfitMargin"))
      .and(query[Option[BigDecimal]]("maxProfitMargin"))
      .and(query[Option[BigDecimal]]("minFreeCashFlow"))
      .and(query[Option[BigDecimal]]("minDividendYield"))
      .and(query[Option[BigDecimal]]("maxDividendYield"))
      .and(query[Option[BigDecimal]]("minRevenueGrowth5Y"))
      .and(query[Option[BigDecimal]]("minEpsGrowth5Y"))
      .mapTo[FinancialFilters]

  private val getStockByTickerEndpoint = Controller.secureEndpoint.get
    .in(stockByTickerPath)
    .out(jsonBody[StockView])

  private val getAllStocksEndpoint = Controller.secureEndpoint.get
    .in("stocks")
    .in(basicInput)
    .in(marketInput)
    .in(scoreInput)
    .in(financialInput)
    .mapInTo[StockQueryParams]
    .out(jsonBody[List[StockView]])

  private val deactivateStockEndpoint = Controller.secureEndpoint.put
    .in(stockByTickerPath / "deactivate")
    .out(statusCode(StatusCode.NoContent))
    .description("Deactivate a stock by setting isActive to false")

  def make[F[_]: Async](stockService: StockService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(StockController[F](stockService, ApiKeyRequirement.Required(config.key)))
}
