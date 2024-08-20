package stockschecker.controllers

import cats.Monad
import cats.effect.kernel.Async
import cats.implicits.toSemigroupKOps
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import stockschecker.services.Services
import org.http4s.HttpRoutes
import org.http4s.server.Router

trait Controllers[F[_]]:
  def companyProfile: Controller[F]
  def stock: Controller[F]
  def command: Controller[F]
  def health: Controller[F]

  def routes(using M: Monad[F]): HttpRoutes[F] =
    Router(
      "api" -> (companyProfile.routes <+> stock.routes <+> command.routes),
      ""    -> health.routes
    )

object Controllers:
  def make[F[_]: Async: Clock](services: Services[F]): F[Controllers[F]] =
    for
      cp <- CompanyProfileController.make(services.companyProfile)
      s  <- StockController.make(services.stock)
      h  <- HealthController.make[F]
      c  <- CommandController.make[F](services.command)
    yield new Controllers[F]:
      override val companyProfile: Controller[F] = cp
      override val health: Controller[F]         = h
      override val stock: Controller[F]          = s
      override val command: Controller[F]        = c
