package stockchecker

import cats.effect.{Async, Resource}
import stockchecker.common.config.AppConfig
import sttp.client3.httpclient.fs2.HttpClientFs2Backend
import sttp.client3.{SttpBackend, SttpBackendOptions}

import scala.concurrent.duration.*

trait Resources[F[_]]:
  def httpBackend: SttpBackend[F, Any]

object Resources {

  private def mkHttpClientBackend[F[_]: Async](timeout: FiniteDuration): Resource[F, SttpBackend[F, Any]] =
    HttpClientFs2Backend.resource[F](SttpBackendOptions(connectionTimeout = timeout, proxy = None))

  def make[F[_]](config: AppConfig)(using F: Async[F]): Resource[F, Resources[F]] =
    for hb <- mkHttpClientBackend(1.minute)
    yield new Resources[F]:
      def httpBackend: SttpBackend[F, Any] = hb
}
