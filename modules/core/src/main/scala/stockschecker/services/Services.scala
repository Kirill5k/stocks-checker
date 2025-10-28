package stockschecker.services

import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.actions.ActionDispatcher
import stockschecker.clients.Clients
import stockschecker.repositories.Repositories

trait Services[F[_]]:
  // def stock: StockService[F] // Disabled until Security model is implemented
  def companyProfile: CompanyProfileService[F]
  def command: CommandService[F]

object Services:
  def make[F[_]: Temporal](clients: Clients[F], repos: Repositories[F], ad: ActionDispatcher[F]): F[Services[F]] =
    for
      // s  <- StockService.make(repos.stock, clients.financialModelingPrep) // Disabled
      cp <- CompanyProfileService.make(repos.companyProfile, clients.marketData)
      c  <- CommandService.make(repos.command, ad)
    yield new Services[F]:
      override def companyProfile: CompanyProfileService[F] = cp
      // override def stock: StockService[F]                   = s // Disabled
      override def command: CommandService[F]               = c
