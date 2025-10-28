package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.domain.{Exchange, Security, Ticker}
import stockschecker.services.SecurityService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class SecurityController[F[_]: Async](
    private val securityService: SecurityService[F]
) extends Controller[F] {

  private val getSecurityByTicker = SecurityController.getSecurityByTickerEndpoint
    .serverLogic { ticker =>
      securityService
        .findByTicker(ticker)
        .mapResponse(identity)
    }

  private val getSecuritiesByExchange = SecurityController.getSecuritiesByExchangeEndpoint
    .serverLogic { exchangeCode =>
      val exchange = exchangeCode match
        case "NASDAQ" | "NSQ" => Exchange.NASDAQ
        case "NYSE" | "NYS"   => Exchange.NYSE
        case _                => Exchange.NASDAQ // default
      securityService
        .findByExchange(exchange)
        .mapResponse(identity)
    }

  private val getAllTickers = SecurityController.getAllTickersEndpoint
    .serverLogic { _ =>
      securityService
        .getAllTickers
        .mapResponse(identity)
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

  private val getSecurityByTickerEndpoint = Controller.publicEndpoint.get
    .in(basePath / path[Ticker]("ticker"))
    .out(jsonBody[Option[Security]])
    .description("Get security by ticker")

  private val getSecuritiesByExchangeEndpoint = Controller.publicEndpoint.get
    .in(basePath / "exchange" / path[String]("exchange"))
    .out(jsonBody[List[Security]])
    .description("Get all securities for an exchange")

  private val getAllTickersEndpoint = Controller.publicEndpoint.get
    .in(basePath / "tickers")
    .out(jsonBody[List[Ticker]])
    .description("Get all tickers")

  def make[F[_]: Async](service: SecurityService[F]): F[Controller[F]] =
    Async[F].pure(SecurityController[F](service))
}

