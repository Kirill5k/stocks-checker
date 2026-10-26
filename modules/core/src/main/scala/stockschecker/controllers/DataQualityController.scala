package stockschecker.controllers

import cats.effect.Async
import org.http4s.HttpRoutes
import stockschecker.common.config.ApiConfig
import stockschecker.domain.DataQualityReport
import stockschecker.services.DataQualityService
import sttp.tapir.*
import sttp.tapir.generic.auto.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

object DataQualityController {
  def make[F[_]: Async](service: DataQualityService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(new Controller[F](ApiKeyRequirement.Required(config.key)) {
      val routes: HttpRoutes[F] = Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
        secured(Controller.secureEndpoint.get.in("data-quality").out(jsonBody[DataQualityReport]))
          .serverLogic(_ => _ => service.report.asResponse)
      )
    })
}
