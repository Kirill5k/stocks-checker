package stockschecker.controllers

import cats.effect.kernel.Async
import cats.syntax.functor.*
import stockschecker.services.Services

trait Controllers[F[_]]:
  def companyProfile: Controller[F]

object Controllers:
  def make[F[_]: Async](services: Services[F]): F[Controllers[F]] =
    for cp <- CompanyProfileController.make(services.companyProfile)
    yield new Controllers[F]:
      override def companyProfile: Controller[F] = cp
