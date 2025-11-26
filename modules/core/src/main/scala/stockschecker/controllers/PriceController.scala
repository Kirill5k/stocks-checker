package stockschecker.controllers

import cats.data.NonEmptyList
import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{PricePerformanceFilter, PricePerformanceSummary, Ticker, TimePeriod}
import stockschecker.services.PriceService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class PriceController[F[_]: Async](
    private val priceService: PriceService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {

  private val getPricePerformanceSummaryByTicker = secured(PriceController.getPricePerformanceSummaryByTickerEndpoint)
    .serverLogic { _ => (ticker, fetchLatest) =>
      priceService
        .findPerformanceSummary(ticker, fetchLatest.getOrElse(false))
        .asResponse
    }

  private val getPricePerformanceSummaries = secured(PriceController.getPricePerformanceSummariesEndpoint)
    .serverLogic { _ => queryParams =>
      val summaries = queryParams.toFilter match
        case Some(filter) => priceService.findPerformanceSummariesBy(filter, queryParams.limit)
        case None         => priceService.getAllPerformanceSummaries(queryParams.limit)

      summaries.asResponse
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getPricePerformanceSummaryByTicker,
        getPricePerformanceSummaries
      )
    )
}

object PriceController extends TapirJsonCirce with SchemaDerivation {

  private val basePath = "price"
  private val performanceSummariesPath = basePath / "performance-summaries"

  private case class PricePerformanceQueryParams(
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
    def toFilter: Option[PricePerformanceFilter] = {
      val filters = List(
        minLatestPrice.map(min => PricePerformanceFilter.PriceAbove(min)),
        maxLatestPrice.map(max => PricePerformanceFilter.PriceBelow(max)),
        minOneMonthChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.OneMonth, min)),
        maxOneMonthChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.OneMonth, max)),
        minThreeMonthChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.ThreeMonth, min)),
        maxThreeMonthChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.ThreeMonth, max)),
        minSixMonthChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.SixMonth, min)),
        maxSixMonthChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.SixMonth, max)),
        minOneYearChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.OneYear, min)),
        maxOneYearChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.OneYear, max)),
        minThreeYearChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.ThreeYear, min)),
        maxThreeYearChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.ThreeYear, max)),
        minFiveYearChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.FiveYear, min)),
        maxFiveYearChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.FiveYear, max)),
        minTenYearChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.TenYear, min)),
        maxTenYearChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.TenYear, max)),
        minMaxChange.map(min => PricePerformanceFilter.PerformanceAbove(TimePeriod.Max, min)),
        maxMaxChange.map(max => PricePerformanceFilter.PerformanceBelow(TimePeriod.Max, max))
      ).flatten
      NonEmptyList.fromList(filters).map(PricePerformanceFilter.Composite(_))
    }
  }

  private val getPricePerformanceSummaryByTickerEndpoint = Controller.secureEndpoint.get
    .in(performanceSummariesPath / path[Ticker])
    .in(query[Option[Boolean]]("fetchLatest"))
    .out(jsonBody[PricePerformanceSummary])
    .description("Get price performance summary by ticker")

  private val getPricePerformanceSummariesEndpoint = Controller.secureEndpoint.get
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
        .mapTo[PricePerformanceQueryParams]
    )
    .out(jsonBody[List[PricePerformanceSummary]])
    .description("Get multiple price performance summaries with optional filters")

  def make[F[_]: Async](service: PriceService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(PriceController[F](service, ApiKeyRequirement.Required(config.key)))
}
