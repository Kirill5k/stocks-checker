package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.domain.{Stock, Ticker}
import stockschecker.services.StockService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class StockController[F[_]: Async](
    private val stockService: StockService[F]
) extends Controller[F] {

  val findStock = StockController.getStockEndpoint
    .serverLogic { (ticker) =>
      stockService
        .get(ticker)
        .mapResponse(identity)
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(findStock)
    )
}

object StockController extends TapirJsonCirce with SchemaDerivation {

  private val basePath = "stocks"

  val getStockEndpoint = Controller.publicEndpoint.get
    .in(basePath / path[Ticker])
    .out(jsonBody[Stock])
    .description("Get company stock by ticker")

  def make[F[_] : Async](service: StockService[F]): F[Controller[F]] =
    Async[F].pure(StockController[F](service))
}
