package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.domain.{Stock, Ticker}
import stockschecker.services.StockService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final class StockController[F[_]: Async](
    stockService: StockService[F]
) extends Controller[F](ApiKeyRequirement.NotRequired) {

  private val getStockByTicker = StockController.getStockByTickerEndpoint
    .serverLogic[F] { ticker =>
      stockService.findByTicker(ticker).mapResponse(identity)
    }

  override val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions)
      .toRoutes(List(getStockByTicker))
}

object StockController extends TapirJsonCirce with SchemaDerivation {

  private val getStockByTickerEndpoint = Controller.publicEndpoint.get
    .in("stocks" / path[Ticker]("ticker"))
    .out(jsonBody[Stock])

  def make[F[_]: Async](stockService: StockService[F]): F[StockController[F]] =
    Async[F].pure(new StockController[F](stockService))
}