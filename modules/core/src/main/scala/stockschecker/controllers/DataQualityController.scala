package stockschecker.controllers

import cats.effect.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.DataQualityReport
import stockschecker.services.DataQualityService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class DataQualityController[F[_]: Async](
    private val service: DataQualityService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {
  private val getReport = secured(DataQualityController.getReportEndpoint)
    .serverLogic(_ => _ => service.report.asResponse)

  override val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(getReport)
}

object DataQualityController extends TapirJsonCirce with SchemaDerivation {
  private val getReportEndpoint = Controller.secureEndpoint.get
    .in("data-quality")
    .out(jsonBody[DataQualityReport])

  def make[F[_]: Async](service: DataQualityService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(DataQualityController[F](service, ApiKeyRequirement.Required(config.key)))
}
