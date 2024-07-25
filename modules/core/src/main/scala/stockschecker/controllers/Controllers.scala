package stockschecker.controllers

import cats.Monad
import cats.effect.kernel.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import stockschecker.services.Services
import org.http4s.HttpRoutes
import org.http4s.server.Router

trait Controllers[F[_]]:
  def companyProfile: Controller[F]
  def health: Controller[F]

  def routes(using M: Monad[F]): HttpRoutes[F] =
    Router(
      "api" -> (companyProfile.routes),
      "" -> health.routes
    )

object Controllers:
  def make[F[_]: Async: Clock](services: Services[F]): F[Controllers[F]] =
    for
      cp <- CompanyProfileController.make(services.companyProfile)
      h  <- HealthController.make[F]
    yield new Controllers[F]:
      override def companyProfile: Controller[F] = cp
      override def health: Controller[F] = h
