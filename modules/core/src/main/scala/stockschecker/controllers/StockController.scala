package stockschecker.controllers

import cats.effect.kernel.Async
import io.circe.Codec as CirceCodec
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.controllers.StockController.{StockQueryParams, StockView}
import stockschecker.domain.{Exchange, SecurityKind, Stock, StockAnalysisMetrics, StockAnalysisScores, StockFilters, StockSortField, Ticker, TimePeriod}
import stockschecker.services.StockService
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
        exchange = params.exchange,
        kind = params.kind,
        country = params.country,
        minMarketCap = params.minMarketCap,
        maxMarketCap = params.maxMarketCap,
        minPrice = params.minPrice,
        maxPrice = params.maxPrice,
        minChange = params.minChange,
        maxChange = params.maxChange,
        period = params.period,
        minOverallScore = params.minOverallScore,
        minCagrScore = params.minCagrScore,
        minVolatilityScore = params.minVolatilityScore,
        sortBy = params.sortBy
      )
      stockService.findAll(filters, params.limit).mapToResponse(_.map(StockView.from))
    }

  override val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(List(getStockByTicker, getAllStocks))
}

object StockController extends TapirJsonCirce with SchemaDerivation {

  final case class StockQueryParams(
      limit: Option[Int],
      exchange: Option[Exchange],
      kind: Option[SecurityKind],
      country: Option[String],
      minMarketCap: Option[Long],
      maxMarketCap: Option[Long],
      minPrice: Option[BigDecimal],
      maxPrice: Option[BigDecimal],
      minChange: Option[BigDecimal],
      maxChange: Option[BigDecimal],
      period: Option[TimePeriod],
      minOverallScore: Option[BigDecimal],
      minCagrScore: Option[BigDecimal],
      minVolatilityScore: Option[BigDecimal],
      sortBy: Option[StockSortField]
  )

  final private case class StockView(
      ticker: Ticker,
      name: String,
      security: StockSecurityView,
      profile: Option[StockCompanyProfileView],
      priceAnalytics: Option[PriceAnalyticsView]
  ) derives CirceCodec.AsObject

  private object StockView:
    def from(stock: Stock): StockView = StockView(
      ticker = stock.security.ticker,
      name = stock.security.name,
      security = StockSecurityView(
        exchange = stock.security.exchange,
        kind = stock.security.kind
      ),
      profile = stock.profile.map(p => StockCompanyProfileView(
        country = p.country,
        industry = p.industry,
        description = p.description,
        website = p.website,
        ipoDate = p.ipoDate,
        currency = p.currency,
        marketCap = p.marketCap,
        lastUpdatedAt = stock.security.companyProfileLastUpdatedAt
      )),
      priceAnalytics = stock.priceAnalytics.map(pa => PriceAnalyticsView(
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
      ))
    )

  final private case class StockSecurityView(
      exchange: Exchange,
      kind: SecurityKind
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

  private val getStockByTickerEndpoint = Controller.secureEndpoint.get
    .in("stocks" / path[Ticker]("ticker"))
    .out(jsonBody[StockView])

  private val getAllStocksEndpoint = Controller.secureEndpoint.get
    .in("stocks")
    .in(query[Option[Int]]("limit"))
    .in(query[Option[Exchange]]("exchange"))
    .in(query[Option[SecurityKind]]("kind"))
    .in(query[Option[String]]("country"))
    .in(query[Option[Long]]("minMarketCap"))
    .in(query[Option[Long]]("maxMarketCap"))
    .in(query[Option[BigDecimal]]("minPrice"))
    .in(query[Option[BigDecimal]]("maxPrice"))
    .in(query[Option[BigDecimal]]("minChange"))
    .in(query[Option[BigDecimal]]("maxChange"))
    .in(query[Option[TimePeriod]]("period"))
    .in(query[Option[BigDecimal]]("minOverallScore"))
    .in(query[Option[BigDecimal]]("minCagrScore"))
    .in(query[Option[BigDecimal]]("minVolatilityScore"))
    .in(query[Option[StockSortField]]("sortBy"))
    .mapInTo[StockQueryParams]
    .out(jsonBody[List[StockView]])

  def make[F[_]: Async](stockService: StockService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(StockController[F](stockService, ApiKeyRequirement.Required(config.key)))
}
