package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{Stock, Ticker}
import stockschecker.services.StockService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class StockController[F[_]: Async](
    private val stockService: StockService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {

  private val getStockByTicker = secured(StockController.getStockByTickerEndpoint)
    .serverLogic { _ => ticker =>
      stockService.findByTicker(ticker).asResponse
    }

  override val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(List(getStockByTicker))
}

object StockController extends TapirJsonCirce with SchemaDerivation {

  private val getStockByTickerEndpoint = Controller.secureEndpoint.get
    .in("stocks" / path[Ticker]("ticker"))
    .out(jsonBody[Stock])

  def make[F[_]: Async](stockService: StockService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(StockController[F](stockService, ApiKeyRequirement.Required(config.key)))
}