package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.domain.{PricePerformanceSummary, Ticker}
import stockschecker.services.PriceService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class PriceController[F[_]: Async](
    private val priceService: PriceService[F]
) extends Controller[F] {

  private val getPricePerformanceSummaryByTicker = PriceController.getPricePerformanceSummaryByTickerEndpoint
    .serverLogic { (ticker, fetchLatest) =>
      priceService
        .findPerformanceSummary(ticker, fetchLatest.getOrElse(false))
        .mapResponse(identity)
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getPricePerformanceSummaryByTicker
      )
    )
}

object PriceController extends TapirJsonCirce with SchemaDerivation {

  private val basePath = "price"

  private val getPricePerformanceSummaryByTickerEndpoint = Controller.publicEndpoint.get
    .in(basePath / "performance-summary" / path[Ticker])
    .in(query[Option[Boolean]]("fetchLatest"))
    .out(jsonBody[PricePerformanceSummary])
    .description("Get price performance summary by ticker")

  def make[F[_]: Async](service: PriceService[F]): F[Controller[F]] =
    Async[F].pure(PriceController[F](service))
}

