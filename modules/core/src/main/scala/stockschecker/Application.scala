package stockschecker

import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{IO, IOApp}
import cats.syntax.foldable.*
import cats.syntax.apply.*
import stockschecker.domain.Provider
import fs2.Stream
import kirill5k.common.http4s.Server
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import stockschecker.actions.{ActionDispatcher, ActionExecutor, ActionHandler, CommandScheduler}
import stockschecker.clients.Clients
import stockschecker.common.config.AppConfig
import stockschecker.controllers.Controllers
import stockschecker.repositories.Repositories
import stockschecker.services.Services

import scala.concurrent.duration.*

object Application extends IOApp.Simple {
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override def runtimeConfig: IORuntimeConfig =
    super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = 10.seconds)

  override val run: IO[Unit] =
    for
      _      <- logger.info(s"starting stocks-checker-core ${sys.env.getOrElse("VERSION", "")}")
      config <- AppConfig.loadDefault[IO]
      _      <- Resources
        .make[IO](config)
        .use: res =>
          for
            repositories     <- Repositories.make(res.mongoDatabase)
            actionDispatcher <- ActionDispatcher.make[IO](repositories.jobs)
            clients          <- Clients.make(config.clients, res.httpBackend, repositories.providerBudgets)
            services         <- Services.make(clients, repositories, actionDispatcher, config.investment)
            controllers      <- Controllers.make(services, config.api)
            actionHandler    <- ActionHandler.make(services)
            actionExecutor   <- ActionExecutor.make(
              repositories.jobs,
              actionHandler,
              services.command.isCurrent,
              (repositories.command.activeRunIds, services.investmentEvaluations.protectedJobIds).mapN(_ ++ _),
              config.actionExecutor
            )
            commandScheduler <- CommandScheduler.make(repositories.command, repositories.jobs)
            restartTime      <- IO.realTimeInstant
            _                <- Provider.values.toList.traverse_(repositories.jobs.resumePaused(_, restartTime))
            _                <- Stream(
              Server.serveEmber(config.server, controllers.routes),
              actionExecutor.run,
              commandScheduler.run
            ).parJoinUnbounded.compile.drain
          yield ()
    yield ()
}
