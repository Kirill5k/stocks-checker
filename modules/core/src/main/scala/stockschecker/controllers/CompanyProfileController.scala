package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.services.CompanyProfileService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class CompanyProfileController[F[_]: Async](
    private val companyProfileService: CompanyProfileService[F]
) extends Controller[F] {

  private val getCompanyProfileByTicker = CompanyProfileController.getCompanyProfileByTickerEndpoint
    .serverLogic { (ticker) =>
      companyProfileService
        .get(ticker)
        .mapResponse(identity)
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getCompanyProfileByTicker
      )
    )
}

object CompanyProfileController extends TapirJsonCirce with SchemaDerivation {

  private val basePath = "company-profiles"

  private val getCompanyProfileByTickerEndpoint = Controller.publicEndpoint.get
    .in(basePath / path[Ticker])
    .out(jsonBody[CompanyProfile])
    .description("Get company profile by ticker")

  def make[F[_]: Async](service: CompanyProfileService[F]): F[Controller[F]] =
    Async[F].pure(CompanyProfileController[F](service))
}
