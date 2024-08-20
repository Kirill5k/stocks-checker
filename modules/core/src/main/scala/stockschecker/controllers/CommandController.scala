package stockschecker.controllers

import cats.effect.Async
import org.http4s.HttpRoutes
import stockschecker.domain.{Command, CommandId}
import stockschecker.services.CommandService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class CommandController[F[_]: Async](
    private val service: CommandService[F]
) extends Controller[F] {

  private val getAllCommands = CommandController.getAllCommandsEndpoint
    .serverLogic { _ =>
      service.getAll
        .mapResponse(identity)
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(getAllCommands)
    )
}

object CommandController extends TapirJsonCirce with SchemaDerivation {
  given Schema[CommandId] = Schema.string
  given Schema[Command]   = Schema.string

  private val basePath = "commands"

  private val getAllCommandsEndpoint = Controller.publicEndpoint.get
    .in(basePath)
    .out(jsonBody[List[Command]])
    .description("Get all commands")

  def make[F[_]: Async](service: CommandService[F]): F[Controller[F]] =
    Async[F].pure(CommandController[F](service))
}
