package stockschecker.services

import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import org.typelevel.log4cats.Logger
import stockschecker.actions.ActionDispatcher
import stockschecker.clients.Clients
import stockschecker.repositories.Repositories

trait Services[F[_]]:
  def security: SecurityService[F]
  def companyProfile: CompanyProfileService[F]
  def command: CommandService[F]

object Services:
  def make[F[_]: Temporal: Logger](clients: Clients[F], repos: Repositories[F], ad: ActionDispatcher[F]): F[Services[F]] =
    for
      s  <- SecurityService.make(repos.security, clients.marketData)
      cp <- CompanyProfileService.make(repos.companyProfile, clients.marketData)
      c  <- CommandService.make(repos.command, ad)
    yield new Services[F]:
      override def security: SecurityService[F]                 = s
      override def companyProfile: CompanyProfileService[F] = cp
      override def command: CommandService[F]               = c
