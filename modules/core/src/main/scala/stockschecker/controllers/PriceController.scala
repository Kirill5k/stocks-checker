package stockschecker.controllers

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{PriceAnalytics, PriceAnalyticsFilter, Ticker, TimePeriod}
import stockschecker.services.PriceService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class PriceController[F[_]: Async](
    private val priceService: PriceService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {

  private val getPriceAnalyticsByTicker = secured(PriceController.getPriceAnalyticsByTickerEndpoint)
    .serverLogic { _ => (ticker, fetchLatest) =>
      priceService
        .findPriceAnalytics(ticker, fetchLatest.getOrElse(false))
        .asResponse
    }

  private val getPriceAnalytics = secured(PriceController.getPriceAnalyticsEndpoint)
    .serverLogic { _ => queryParams =>
      val analytics = queryParams.toFilter match
        case Some(filter) => priceService.findPriceAnalyticsBy(filter, queryParams.limit)
        case None         => priceService.getAllPriceAnalytics(queryParams.limit)

      analytics.asResponse
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getPriceAnalyticsByTicker,
        getPriceAnalytics
      )
    )
}

object PriceController extends TapirJsonCirce with SchemaDerivation {

  private val basePath                 = "price"
  private val performanceSummariesPath = basePath / "performance-summaries"

  private case class PriceAnalyticsQueryParams(
      minLatestPrice: Option[BigDecimal] = None,
      maxLatestPrice: Option[BigDecimal] = None,
      minOneMonthChange: Option[BigDecimal] = None,
      maxOneMonthChange: Option[BigDecimal] = None,
      minThreeMonthChange: Option[BigDecimal] = None,
      maxThreeMonthChange: Option[BigDecimal] = None,
      minSixMonthChange: Option[BigDecimal] = None,
      maxSixMonthChange: Option[BigDecimal] = None,
      minOneYearChange: Option[BigDecimal] = None,
      maxOneYearChange: Option[BigDecimal] = None,
      minThreeYearChange: Option[BigDecimal] = None,
      maxThreeYearChange: Option[BigDecimal] = None,
      minFiveYearChange: Option[BigDecimal] = None,
      maxFiveYearChange: Option[BigDecimal] = None,
      minTenYearChange: Option[BigDecimal] = None,
      maxTenYearChange: Option[BigDecimal] = None,
      minMaxChange: Option[BigDecimal] = None,
      maxMaxChange: Option[BigDecimal] = None,
      limit: Option[Int] = None
  ) {
    def toFilter: Option[PriceAnalyticsFilter] = {
      val filters = List(
        minLatestPrice.map(min => PriceAnalyticsFilter.PriceAbove(min)),
        maxLatestPrice.map(max => PriceAnalyticsFilter.PriceBelow(max)),
        minOneMonthChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.OneMonth, min)),
        maxOneMonthChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.OneMonth, max)),
        minThreeMonthChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.ThreeMonth, min)),
        maxThreeMonthChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.ThreeMonth, max)),
        minSixMonthChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.SixMonth, min)),
        maxSixMonthChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.SixMonth, max)),
        minOneYearChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.OneYear, min)),
        maxOneYearChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.OneYear, max)),
        minThreeYearChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.ThreeYear, min)),
        maxThreeYearChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.ThreeYear, max)),
        minFiveYearChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.FiveYear, min)),
        maxFiveYearChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.FiveYear, max)),
        minTenYearChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.TenYear, min)),
        maxTenYearChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.TenYear, max)),
        minMaxChange.map(min => PriceAnalyticsFilter.PerformanceAbove(TimePeriod.Max, min)),
        maxMaxChange.map(max => PriceAnalyticsFilter.PerformanceBelow(TimePeriod.Max, max))
      ).flatten
      NonEmptyList.fromList(filters).map(PriceAnalyticsFilter.Composite(_))
    }
  }

  private val getPriceAnalyticsByTickerEndpoint = Controller.secureEndpoint.get
    .in(performanceSummariesPath / path[Ticker])
    .in(query[Option[Boolean]]("fetchLatest"))
    .out(jsonBody[PriceAnalytics])
    .description("Get price analytics by ticker")

  private val getPriceAnalyticsEndpoint = Controller.secureEndpoint.get
    .in(performanceSummariesPath)
    .in(
      query[Option[BigDecimal]]("minLatestPrice")
        .and(query[Option[BigDecimal]]("maxLatestPrice"))
        .and(query[Option[BigDecimal]]("minOneMonthChange"))
        .and(query[Option[BigDecimal]]("maxOneMonthChange"))
        .and(query[Option[BigDecimal]]("minThreeMonthChange"))
        .and(query[Option[BigDecimal]]("maxThreeMonthChange"))
        .and(query[Option[BigDecimal]]("minSixMonthChange"))
        .and(query[Option[BigDecimal]]("maxSixMonthChange"))
        .and(query[Option[BigDecimal]]("minOneYearChange"))
        .and(query[Option[BigDecimal]]("maxOneYearChange"))
        .and(query[Option[BigDecimal]]("minThreeYearChange"))
        .and(query[Option[BigDecimal]]("maxThreeYearChange"))
        .and(query[Option[BigDecimal]]("minFiveYearChange"))
        .and(query[Option[BigDecimal]]("maxFiveYearChange"))
        .and(query[Option[BigDecimal]]("minTenYearChange"))
        .and(query[Option[BigDecimal]]("maxTenYearChange"))
        .and(query[Option[BigDecimal]]("minMaxChange"))
        .and(query[Option[BigDecimal]]("maxMaxChange"))
        .and(query[Option[Int]]("limit"))
        .mapTo[PriceAnalyticsQueryParams]
    )
    .out(jsonBody[List[PriceAnalytics]])
    .description("Get multiple price analytics with optional filters")

  def make[F[_]: Async](service: PriceService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(PriceController[F](service, ApiKeyRequirement.Required(config.key)))
}
