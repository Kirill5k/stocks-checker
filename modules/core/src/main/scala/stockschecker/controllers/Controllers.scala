package stockschecker.controllers

import cats.Monad
import cats.effect.kernel.Async
import cats.syntax.functor.*
import stockschecker.services.Services
import org.http4s.HttpRoutes
import org.http4s.server.Router

trait Controllers[F[_]]:
  def companyProfile: Controller[F]

  def routes(using M: Monad[F]): HttpRoutes[F] =
    Router(
      "api" -> (companyProfile.routes)
    )

object Controllers:
  def make[F[_]: Async](services: Services[F]): F[Controllers[F]] =
    for cp <- CompanyProfileController.make(services.companyProfile)
    yield new Controllers[F]:
      override def companyProfile: Controller[F] = cp
