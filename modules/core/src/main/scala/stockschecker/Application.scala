package stockschecker

import cats.effect.{IO, IOApp}
import kirill5k.common.http4s.Server
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import stockschecker.actions.{ActionDispatcher, ActionExecutor}
import stockschecker.clients.Clients
import stockschecker.common.config.AppConfig
import stockschecker.controllers.Controllers
import stockschecker.repositories.Repositories
import stockschecker.services.Services

object Application extends IOApp.Simple {
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override val run: IO[Unit] =
    for
      config <- AppConfig.loadDefault[IO]
      _ <- Resources
        .make[IO](config)
        .use: res =>
          for
            actionDispatcher <- ActionDispatcher.make[IO]
            clients          <- Clients.make(config.clients, res.httpBackend)
            repositories     <- Repositories.make(res.mongoDatabase)
            services         <- Services.make(clients, repositories, actionDispatcher)
            controllers      <- Controllers.make(services)
            actionExecutor   <- ActionExecutor.make(actionDispatcher, services)
            _ <- Server
              .serveEmber(config.server, controllers.routes)
              .concurrently(actionExecutor.run)
              .compile
              .drain
          yield ()
    yield ()
}
