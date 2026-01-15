package stockschecker.controllers

import cats.effect.Async
import io.circe.Codec
import org.http4s.HttpRoutes
import stockschecker.actions.Action
import stockschecker.common.config.ApiConfig
import stockschecker.controllers.CommandController.CreateCommandResponse
import stockschecker.domain.{Command, CommandId, CreateCommand, Schedule, UpdateCommand}
import stockschecker.services.CommandService
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class CommandController[F[_]: Async](
    private val service: CommandService[F],
    apiKeyRequirement: ApiKeyRequirement
) extends Controller[F](apiKeyRequirement) {

  private val getAllCommands = secured(CommandController.getAllCommandsEndpoint)
    .serverLogic { _ => _ =>
      service.getAll.asResponse
    }

  private val createCommand = secured(CommandController.createCommandEndpoint)
    .serverLogic { _ => req =>
      service
        .create(CreateCommand(req.action, req.schedule, req.maxExecutions))
        .mapToResponse(cmd => CreateCommandResponse(cmd.id))
    }

  private val activateCommand = secured(CommandController.activateCommandEndpoint)
    .serverLogic { _ =>
      { case (cid, req) =>
        service
          .activate(cid, req.isActive)
          .asVoidResponse
      }
    }

  private val updateCommand = secured(CommandController.updateCommandEndpoint)
    .serverLogic { _ =>
      { case (cid, req) =>
        service
          .update(UpdateCommand(cid, req.isActive, req.action, req.schedule, req.maxExecutions))
          .asResponse
      }
    }

  private val executeManuallyCommand = secured(CommandController.executeManuallyCommandEndpoint)
    .serverLogic { _ => cid =>
      service
        .executeManually(cid)
        .asVoidResponse
    }

  val routes: HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getAllCommands,
        createCommand,
        activateCommand,
        updateCommand,
        executeManuallyCommand
      )
    )
}

object CommandController extends TapirJsonCirce with SchemaDerivation {
  given Schema[CommandId] = Schema.string
  given Schema[Action]    = Schema.string
  given Schema[Schedule]  = Schema.string

  private val basePath      = "commands"
  private val commandIdPath = basePath / path[String]
    .validate(Controller.validId)
    .map((s: String) => CommandId(s))(_.value)
    .name("command-id")

  private val getAllCommandsEndpoint = Controller.secureEndpoint.get
    .in(basePath)
    .out(jsonBody[List[Command]])
    .description("Get all commands")

  final private case class CreateCommandRequest(
      action: Action,
      schedule: Schedule,
      maxExecutions: Option[Int] = None
  ) derives Codec.AsObject

  final private case class CreateCommandResponse(
      id: CommandId
  ) derives Codec.AsObject

  private val createCommandEndpoint = Controller.secureEndpoint.post
    .in(basePath)
    .in(jsonBody[CreateCommandRequest])
    .out(jsonBody[CreateCommandResponse].and(statusCode(StatusCode.Created)))
    .description("Create new command")

  final private case class ActivateCommandRequest(
      isActive: Boolean
  ) derives Codec.AsObject

  private val activateCommandEndpoint = Controller.secureEndpoint.put
    .in(commandIdPath / "active")
    .in(jsonBody[ActivateCommandRequest])
    .out(statusCode(StatusCode.NoContent))
    .description("Change active status of a command")

  final private case class UpdateCommandRequest(
      isActive: Boolean,
      action: Action,
      schedule: Schedule,
      maxExecutions: Option[Int]
  ) derives Codec.AsObject

  private val updateCommandEndpoint = Controller.secureEndpoint.put
    .in(commandIdPath)
    .in(jsonBody[UpdateCommandRequest])
    .out(jsonBody[Command])
    .description("Update an existing command")

  private val executeManuallyCommandEndpoint = Controller.secureEndpoint.post
    .in(commandIdPath / "execute")
    .out(statusCode(StatusCode.NoContent))
    .description("Manually execute a command without affecting its schedule or execution count")

  def make[F[_]: Async](config: ApiConfig, service: CommandService[F]): F[Controller[F]] =
    Async[F].pure(CommandController[F](service, ApiKeyRequirement.Required(config.key)))
}
