package stockschecker.services

import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.actions.ActionDispatcher
import stockschecker.clients.Clients
import stockschecker.repositories.Repositories

trait Services[F[_]]:
  def stock: StockService[F]
  def companyProfile: CompanyProfileService[F]
  def command: CommandService[F]

object Services:
  def make[F[_]](clients: Clients[F], repos: Repositories[F], ad: ActionDispatcher[F])(using F: Temporal[F]): F[Services[F]] =
    for
      s  <- StockService.make(repos.stock, clients.financialModelingPrep)
      cp <- CompanyProfileService.make(repos.companyProfile, clients.financialModelingPrep)
      c  <- CommandService.make(repos.command, ad)
    yield new Services[F]:
      override def companyProfile: CompanyProfileService[F] = cp
      override def stock: StockService[F]                   = s
      override def command: CommandService[F]               = c
