package stockschecker.controllers

import cats.Monad
import cats.effect.kernel.Async
import cats.implicits.toSemigroupKOps
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import stockschecker.common.config.ApiConfig
import stockschecker.services.Services
import org.http4s.HttpRoutes
import org.http4s.server.Router

trait Controllers[F[_]]:
  def security: Controller[F]
  def companyProfile: Controller[F]
  def price: Controller[F]
  def command: Controller[F]
  def health: Controller[F]
  def stock: Controller[F]

  def routes(using M: Monad[F]): HttpRoutes[F] =
    Router(
      "api" -> (security.routes <+> companyProfile.routes <+> price.routes <+> command.routes <+> stock.routes),
      ""    -> health.routes
    )

object Controllers:
  def make[F[_]: {Async, Clock}](services: Services[F], apiConfig: ApiConfig): F[Controllers[F]] =
    for
      s  <- SecurityController.make(services.security, apiConfig)
      p  <- PriceController.make(services.price, apiConfig)
      cp <- CompanyProfileController.make(services.companyProfile, apiConfig)
      h  <- HealthController.make[F]
      c  <- CommandController.make[F](apiConfig, services.command)
      st <- StockController.make(services.stock, apiConfig)
    yield new Controllers[F]:
      override val security: Controller[F]       = s
      override val price: Controller[F]          = p
      override val companyProfile: Controller[F] = cp
      override val health: Controller[F]         = h
      override val command: Controller[F]        = c
      override val stock: Controller[F]          = st
