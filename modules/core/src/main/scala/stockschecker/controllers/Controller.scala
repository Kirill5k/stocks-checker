package stockschecker.controllers

import cats.MonadThrow
import cats.effect.Sync
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import io.circe.Codec
import mongo4cats.bson.ObjectId
import org.http4s.HttpRoutes
import stockschecker.domain.errors.AppError
import sttp.tapir.*
import sttp.model.StatusCode
import sttp.tapir.DecodeResult.Error.JsonDecodeException
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.PartialServerEndpoint
import sttp.tapir.server.http4s.Http4sServerOptions
import sttp.tapir.server.interceptor.DecodeFailureContext
import sttp.tapir.server.interceptor.exception.{ExceptionContext, ExceptionHandler}
import sttp.tapir.server.model.ValuedEndpointOutput
import sttp.tapir.server.interceptor.decodefailure.DecodeFailureHandler

final case class ErrorResponse(message: String) derives Codec.AsObject

sealed trait ApiKeyRequirement

object ApiKeyRequirement:
  case object NotRequired                                        extends ApiKeyRequirement
  final class Required private (private val expectedKey: String) extends ApiKeyRequirement {
    def authenticate(providedKey: String): Boolean =
      java.security.MessageDigest.isEqual(
        expectedKey.getBytes("UTF-8"),
        providedKey.getBytes("UTF-8")
      )
  }

  object Required:
    def apply(key: String): Required = new Required(key)

trait Controller[F[_]](protected val apiKeyRequirement: ApiKeyRequirement) extends TapirJsonCirce with SchemaDerivation {

  def routes: HttpRoutes[F]

  protected def secured[I, O](
      endpoint: Endpoint[Option[String], I, (StatusCode, ErrorResponse), O, Any]
  ): PartialServerEndpoint[Option[String], Unit, I, (StatusCode, ErrorResponse), O, Any, F] =
    endpoint.serverSecurityLogicPure { providedApiKey =>
      apiKeyRequirement match
        case req: ApiKeyRequirement.Required =>
          Either.cond(providedApiKey.exists(req.authenticate), (), (StatusCode.Unauthorized, ErrorResponse("Invalid API key")))
        case ApiKeyRequirement.NotRequired =>
          Left((StatusCode.Forbidden, ErrorResponse("API key authentication is not configured for this endpoint")))
    }

  extension [A](fa: F[A])(using F: MonadThrow[F])
    def asVoidResponse: F[Either[(StatusCode, ErrorResponse), Unit]] =
      mapToResponse(_ => ())
    def asResponse: F[Either[(StatusCode, ErrorResponse), A]] =
      mapToResponse(identity)
    def mapToResponse[B](fab: A => B): F[Either[(StatusCode, ErrorResponse), B]] =
      fa
        .map(fab(_).asRight[(StatusCode, ErrorResponse)])
        .handleError(e => Controller.mapError(e).asLeft[B])

}

object Controller extends TapirJsonCirce with SchemaDerivation {
  val validId: Validator[String] = Validator.custom { id =>
    if ObjectId.isValid(id) then ValidationResult.Valid else ValidationResult.Invalid(s"Invalid hexadecimal representation of an id: $id")
  }

  private val error = statusCode.and(jsonBody[ErrorResponse])

  val publicEndpoint: PublicEndpoint[Unit, (StatusCode, ErrorResponse), Unit, Any] =
    endpoint.errorOut(error)

  val secureEndpoint: Endpoint[Option[String], Unit, (StatusCode, ErrorResponse), Unit, Any] =
    publicEndpoint.securityIn(auth.apiKey(header[Option[String]]("X-API-Key")))

  def serverOptions[F[_]](using F: Sync[F]): Http4sServerOptions[F] = {
    val errorEndpointOut = (e: Throwable) => Some(ValuedEndpointOutput(error, Controller.mapError(e)))
    Http4sServerOptions.customiseInterceptors
      .exceptionHandler(ExceptionHandler.pure((ctx: ExceptionContext) => errorEndpointOut(ctx.e)))
      .decodeFailureHandler(DecodeFailureHandler.pure { (ctx: DecodeFailureContext) =>
        ctx.failure match
          case DecodeResult.Error(_, e)     => errorEndpointOut(e)
          case DecodeResult.InvalidValue(e) =>
            val msgs = e.collect { case ValidationError(_, _, _, Some(msg)) => msg }
            errorEndpointOut(AppError.FailedValidation(msgs.mkString(", ")))
          case _ => None
      })
      .options
  }

  private val FailedRegexValidation = "Predicate failed: \"(.*)\"\\.matches\\(.*\\)\\.".r
  private val NullFieldValidation   = "Attempt to decode value on failed cursor".r
  private val EmptyFieldValidation  = "Predicate isEmpty\\(\\) did not fail\\.".r
  private val IdValidation          = "Predicate failed: \\((.*) is valid id\\).".r

  private def formatJsonError(err: JsonDecodeException): String =
    err.errors
      .map { je =>
        je.msg match
          case FailedRegexValidation(value) => s"$value is not a valid ${je.path.head.name}"
          case NullFieldValidation()        => s"${je.path.head.name} is required"
          case EmptyFieldValidation()       => s"${je.path.head.name} must not be empty"
          case IdValidation(value)          => s"$value is not a valid ${je.path.head.name}"
          case "Missing required field"     => s"${je.msg}: ${je.path.map(_.name).mkString(", ")}"
          case msg if je.path.isEmpty       => s"Invalid message body: Could not decode $msg json"
          case msg                          => msg
      }
      .mkString(", ")

  def mapError(error: Throwable): (StatusCode, ErrorResponse) = error match
    case err: AppError.Conflict =>
      (StatusCode.Conflict, ErrorResponse(err.getMessage))
    case err: AppError.BadReq =>
      (StatusCode.BadRequest, ErrorResponse(err.getMessage))
    case err: AppError.NotFound =>
      (StatusCode.NotFound, ErrorResponse(err.getMessage))
    case err: AppError.Forbidden =>
      (StatusCode.Forbidden, ErrorResponse(err.getMessage))
    case err: AppError.Unauth =>
      (StatusCode.Unauthorized, ErrorResponse(err.getMessage))
    case err: AppError.Unprocessable =>
      (StatusCode.UnprocessableEntity, ErrorResponse(err.getMessage))
    case err: JsonDecodeException =>
      (StatusCode.UnprocessableEntity, ErrorResponse(formatJsonError(err)))
    case err =>
      (StatusCode.InternalServerError, ErrorResponse(err.getMessage))
}
