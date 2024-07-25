package stockschecker.controllers

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import io.circe.Codec
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import org.http4s.HttpRoutes
import sttp.capabilities.fs2.Fs2Streams
import sttp.tapir.*
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.json.circe.TapirJsonCirce
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.net.InetAddress
import java.time.Instant

final class HealthController[F[_]: Async](
    private val service: String,
    private val startupTime: Instant,
    private val ipAddress: String,
    private val appVersion: Option[String]
)(using
    clock: Clock[F]
) extends Controller[F] {

  private val statusEndpoint: ServerEndpoint[Fs2Streams[F], F] =
    HealthController.statusEndpoint
      .serverLogicSuccess { req =>
        clock
          .durationBetweenNowAnd(startupTime)
          .map { uptime =>
            HealthController.AppStatus(
              service,
              startupTime,
              uptime.toReadableString,
              appVersion,
              ipAddress
            )
          }
      }

  val routes: HttpRoutes[F] = Http4sServerInterpreter[F]().toRoutes(statusEndpoint)
}

object HealthController extends TapirJsonCirce with SchemaDerivation {

  final case class AppStatus(
      service: String,
      startupTime: Instant,
      upTime: String,
      appVersion: Option[String],
      serverIpAddress: String
  ) derives Codec.AsObject

  val statusEndpoint = infallibleEndpoint.get
    .in("health" / "status")
    .out(jsonBody[AppStatus])

  def make[F[_]](using F: Async[F], C: Clock[F]): F[Controller[F]] =
    for
      now     <- C.now
      ip      <- F.blocking(InetAddress.getLocalHost.getHostAddress)
      version <- F.delay(sys.env.get("VERSION"))
    yield HealthController[F]("stocks-checker", now, ip, version)
}
