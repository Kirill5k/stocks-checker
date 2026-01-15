package stockschecker.controllers

import cats.effect.kernel.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.{CompanyProfile, Ticker}
import stockschecker.services.CompanyProfileService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class CompanyProfileController[F[_]: Async](
    private val companyProfileService: CompanyProfileService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {

  private val getCompanyProfileByTicker = secured(CompanyProfileController.getCompanyProfileByTickerEndpoint)
    .serverLogic { _ =>
      { case (ticker, fetchLatest) =>
        companyProfileService
          .get(ticker, fetchLatest.getOrElse(false))
          .asResponse
      }
    }

  private val getCompanyProfile = secured(CompanyProfileController.getCompanyProfileEndpoint)
    .serverLogic { _ => limit =>
      companyProfileService.getAll(limit).asResponse
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getCompanyProfileByTicker,
        getCompanyProfile
      )
    )
}

object CompanyProfileController extends TapirJsonCirce with SchemaDerivation {

  private val basePath = "company-profiles"

  private val getCompanyProfileByTickerEndpoint = Controller.secureEndpoint.get
    .in(basePath / path[Ticker])
    .in(query[Option[Boolean]]("fetchLatest"))
    .out(jsonBody[CompanyProfile])
    .description("Get company profile by ticker")

  private val getCompanyProfileEndpoint = Controller.secureEndpoint.get
    .in(basePath)
    .in(query[Option[Int]]("limit"))
    .out(jsonBody[List[CompanyProfile]])
    .description("Get all company profiles")

  def make[F[_]: Async](service: CompanyProfileService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(CompanyProfileController[F](service, ApiKeyRequirement.Required(config.key)))
}
