package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{Exchange, Security, Ticker}
import stockschecker.services.SecurityService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class SecurityController[F[_]: Async](
    private val securityService: SecurityService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {

  private val getSecurityByTicker = secured(SecurityController.getSecurityByTickerEndpoint)
    .serverLogic { _ => ticker =>
      securityService
        .find(ticker)
        .asResponse
    }

  private val getSecuritiesByExchange = secured(SecurityController.getSecuritiesByExchangeEndpoint)
    .serverLogic { _ => exchange =>
      securityService
        .findByExchange(exchange)
        .asResponse
    }

  private val getAllTickers = secured(SecurityController.getAllTickersEndpoint)
    .serverLogic { _ => _ =>
      securityService.getAllTickers.asResponse
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getSecurityByTicker,
        getSecuritiesByExchange,
        getAllTickers
      )
    )
}

object SecurityController extends TapirJsonCirce with SchemaDerivation {

  private val basePath = "securities"

  private val getSecurityByTickerEndpoint = Controller.secureEndpoint.get
    .in(basePath / path[Ticker]("ticker"))
    .out(jsonBody[Security])
    .description("Get security by ticker")

  private val getSecuritiesByExchangeEndpoint = Controller.secureEndpoint.get
    .in(basePath / "exchange" / path[Exchange]("exchange"))
    .out(jsonBody[List[Security]])
    .description("Get all securities for an exchange")

  private val getAllTickersEndpoint = Controller.secureEndpoint.get
    .in(basePath / "tickers")
    .out(jsonBody[List[Ticker]])
    .description("Get all tickers")

  def make[F[_]: Async](service: SecurityService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(SecurityController[F](service, ApiKeyRequirement.Required(config.key)))
}
