package stockschecker.controllers

import cats.effect.Async
import org.http4s.HttpRoutes
import stockschecker.actions.Action
import stockschecker.common.config.ApiConfig
import stockschecker.domain.*
import stockschecker.domain.errors.AppError
import stockschecker.services.JobService
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerInterpreter

final private class JobController[F[_]: Async](service: JobService[F], apiKeyRequirement: ApiKeyRequirement)
    extends Controller[F](apiKeyRequirement):
  private val list = secured(JobController.listEndpoint).serverLogic { _ =>
    { case (limit, commandId, state, actionKind) =>
      service.list(limit.getOrElse(20), JobFilter(commandId, state, actionKind)).asResponse
    }
  }
  private val get = secured(JobController.getEndpoint).serverLogic(_ => id => service.get(id).asResponse)
  override val routes: HttpRoutes[F] = Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(List(list, get))

object JobController extends TapirJsonCirce with SchemaDerivation:
  private given Schema[Action] = Schema.string
  private given Schema[CommandId] = Schema.string
  private given Schema[JobState] = Schema.string
  private given Codec.PlainCodec[CommandId] = Codec.string.validate(Controller.validId).map(CommandId(_))(_.value)
  private given Codec.PlainCodec[JobState] = Codec.string.mapDecode { value =>
    JobState.values.find(_.value == value).fold[DecodeResult[JobState]](
      DecodeResult.Error(value, AppError.FailedValidation(s"Unknown job state: $value"))
    )(DecodeResult.Value(_))
  }(_.value)

  private val validLimit = Validator.custom[Option[Int]] { limit =>
    if limit.forall(value => value >= 1 && value <= 100) then ValidationResult.Valid
    else ValidationResult.Invalid("limit must be between 1 and 100")
  }
  private val validActionKind = Validator.custom[Option[String]] { kind =>
    if kind.exists(_.trim.isEmpty) then ValidationResult.Invalid("actionKind must not be empty")
    else if kind.exists(_.length > 100) then ValidationResult.Invalid("actionKind must not exceed 100 characters")
    else ValidationResult.Valid
  }
  private val validJobId = Validator.custom[JobId] { id =>
    if id.value.isEmpty then ValidationResult.Invalid("Job id must not be empty")
    else if id.value.length > 1024 then ValidationResult.Invalid("Job id must not exceed 1024 characters")
    else ValidationResult.Valid
  }

  private val listEndpoint = Controller.secureEndpoint.get
    .in("jobs")
    .in(query[Option[Int]]("limit").validate(validLimit))
    .in(query[Option[CommandId]]("commandId"))
    .in(query[Option[JobState]]("state"))
    .in(query[Option[String]]("actionKind").validate(validActionKind))
    .out(jsonBody[List[JobView]])

  private val getEndpoint = Controller.secureEndpoint.get
    .in("jobs" / "status")
    .in(query[JobId]("id").validate(validJobId))
    .out(jsonBody[JobView])

  def make[F[_]: Async](service: JobService[F], config: ApiConfig): F[Controller[F]] =
    Async[F].pure(JobController(service, ApiKeyRequirement.Required(config.key)))
