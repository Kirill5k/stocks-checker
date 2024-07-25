package stockschecker.controllers

import cats.MonadThrow
import cats.effect.Sync
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import io.circe.Codec
import org.http4s.HttpRoutes
import stockschecker.domain.errors.AppError
import sttp.tapir.*
import sttp.model.StatusCode
import sttp.tapir.DecodeResult.Error.JsonDecodeException
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.http4s.Http4sServerOptions
import sttp.tapir.server.interceptor.DecodeFailureContext
import sttp.tapir.server.interceptor.exception.{ExceptionContext, ExceptionHandler}
import sttp.tapir.server.model.ValuedEndpointOutput
import sttp.tapir.server.interceptor.decodefailure.DecodeFailureHandler

final case class ErrorResponse(message: String) derives Codec.AsObject

trait Controller[F[_]] extends TapirJsonCirce with SchemaDerivation {

  def routes: HttpRoutes[F]

  extension [A](fa: F[A])(using F: MonadThrow[F])
    def voidResponse: F[Either[(StatusCode, ErrorResponse), Unit]] = mapResponse(_ => ())
    def mapResponse[B](fab: A => B): F[Either[(StatusCode, ErrorResponse), B]] =
      fa
        .map(fab(_).asRight[(StatusCode, ErrorResponse)])
        .handleError(e => Controller.mapError(e).asLeft[B])

}

object Controller extends TapirJsonCirce with SchemaDerivation {

  private val error = statusCode.and(jsonBody[ErrorResponse])

  val publicEndpoint: PublicEndpoint[Unit, (StatusCode, ErrorResponse), Unit, Any] =
    endpoint.errorOut(error)

  def serverOptions[F[_]](using F: Sync[F]): Http4sServerOptions[F] = {
    val errorEndpointOut = (e: Throwable) => Some(ValuedEndpointOutput(error, Controller.mapError(e)))
    Http4sServerOptions.customiseInterceptors
      .exceptionHandler(ExceptionHandler.pure((ctx: ExceptionContext) => errorEndpointOut(ctx.e)))
      .decodeFailureHandler(DecodeFailureHandler.pure { (ctx: DecodeFailureContext) =>
        ctx.failure match
          case DecodeResult.Error(_, e) => errorEndpointOut(e)
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
          case msg if je.path.isEmpty       => s"Invalid message body: Could not decode $msg json"
          case msg                          => msg
      }
      .mkString(", ")

  def mapError(error: Throwable): (StatusCode, ErrorResponse) =
    error match
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
