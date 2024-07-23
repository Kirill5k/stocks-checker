package stockschecker.services

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.clients.Clients
import stockschecker.repositories.Repositories

trait Services[F[_]]:
  def stock: StockService[F]
  def companyProfile: CompanyProfileService[F]

object Services:
  def make[F[_]](clients: Clients[F], repos: Repositories[F])(using F: MonadThrow[F]): F[Services[F]] =
    for
      s  <- StockService.make(repos.stock, clients.financialModelingPrep)
      cp <- CompanyProfileService.make(repos.companyProfile, clients.financialModelingPrep)
    yield new Services[F]:
      override def companyProfile: CompanyProfileService[F] = cp
      override def stock: StockService[F]                   = s
