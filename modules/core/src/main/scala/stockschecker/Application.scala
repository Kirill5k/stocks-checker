package stockschecker

import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{IO, IOApp}
import fs2.Stream
import kirill5k.common.http4s.Server
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger
import stockschecker.actions.{ActionDispatcher, ActionExecutor, CommandScheduler}
import stockschecker.clients.Clients
import stockschecker.common.config.AppConfig
import stockschecker.controllers.Controllers
import stockschecker.repositories.Repositories
import stockschecker.services.{InvestmentResearchRunner, Services}

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
            actionDispatcher <- ActionDispatcher.durable[IO](repositories.jobs)
            clients          <- Clients.make(config.clients, res.httpBackend, repositories.dataQuality, repositories.providerBudgets)
            services         <- Services.make(clients, repositories, actionDispatcher, config.investment)
            controllers      <- Controllers.make(services, config.api)
            actionExecutor   <- ActionExecutor.make(repositories.jobs, services, config.actionExecutor)
            restartTime      <- IO.realTimeInstant
            _                <- repositories.jobs.resumePaused("Finnhub", restartTime)
            _                <- repositories.jobs.resumePaused("TwelveData", restartTime)
            _                <- Stream(
              Server.serveEmber(config.server, controllers.routes),
              actionExecutor.run,
              CommandScheduler.run(repositories.command, repositories.jobs),
              InvestmentResearchRunner.run(services.investmentScreens, config.investment)
            ).parJoinUnbounded.compile.drain
          yield ()
    yield ()
}
