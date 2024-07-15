package stockchecker

import cats.effect.{IO, IOApp}
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import stockchecker.clients.Clients
import stockchecker.common.config.AppConfig

object Application extends IOApp.Simple {
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override val run: IO[Unit] =
    for
      config <- AppConfig.loadDefault[IO]
      _ <- Resources
        .make[IO](config)
        .use: res =>
          for _ <- Clients.make(config.clients, res.httpBackend)
          yield ()
    yield ()
}
