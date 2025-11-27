package stockschecker.controllers

import cats.effect.kernel.Async
import io.circe.Codec as CirceCodec
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.controllers.StockController.StockView
import stockschecker.domain.{Exchange, SecurityKind, Stock, Ticker}
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

  override val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(List(getStockByTicker))
}

object StockController extends TapirJsonCirce with SchemaDerivation {

  final private case class StockView(
      ticker: Ticker,
      name: String,
      security: StockSecurityView,
      profile: Option[StockCompanyProfileView],
      performanceSummary: Option[StockPricePerformanceSummaryView]
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
      performanceSummary = stock.performanceSummary.map(ps => StockPricePerformanceSummaryView(
        latestPrice = ps.latestPrice,
        latestPriceDate = ps.latestPriceDate,
        oneMonthChange = ps.oneMonthChange,
        threeMonthChange = ps.threeMonthChange,
        sixMonthChange = ps.sixMonthChange,
        oneYearChange = ps.oneYearChange,
        threeYearChange = ps.threeYearChange,
        fiveYearChange = ps.fiveYearChange,
        tenYearChange = ps.tenYearChange,
        maxChange = ps.maxChange,
        lastUpdatedAt = stock.profile.flatMap(_.pricePerformanceLastUpdatedAt)
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
      ipoDate: LocalDate,
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
      maxChange: Option[BigDecimal],
      lastUpdatedAt: Option[Instant]
  ) derives CirceCodec.AsObject

  private val getStockByTickerEndpoint = Controller.secureEndpoint.get
    .in("stocks" / path[Ticker]("ticker"))
    .out(jsonBody[StockView])

  def make[F[_]: Async](stockService: StockService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(StockController[F](stockService, ApiKeyRequirement.Required(config.key)))
}
