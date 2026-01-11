package stockschecker.services

import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.actions.ActionDispatcher
import stockschecker.clients.Clients
import stockschecker.repositories.Repositories

trait Services[F[_]]:
  def security: SecurityService[F]
  def companyProfile: CompanyProfileService[F]
  def price: PriceService[F]
  def command: CommandService[F]
  def stock: StockService[F]
  def financialMetrics: FinancialMetricsService[F]

object Services:
  def make[F[_]: {Temporal, Clock, Logger}](clients: Clients[F], repos: Repositories[F], ad: ActionDispatcher[F]): F[Services[F]] =
    for
      s  <- SecurityService.make(repos.security, clients.marketData)
      cp <- CompanyProfileService.make(repos.companyProfile, clients.marketData, ad)
      p  <- PriceService.make(repos.priceAnalytics, repos.latestPrice, clients.marketData, ad)
      c  <- CommandService.make(repos.command, ad)
      st <- StockService.make(repos.stock)
      fm <- FinancialMetricsService.make(repos.financialMetrics, clients.marketData, ad)
    yield new Services[F]:
      override def security: SecurityService[F]                 = s
      override def companyProfile: CompanyProfileService[F]     = cp
      override def price: PriceService[F]                       = p
      override def command: CommandService[F]                   = c
      override def stock: StockService[F]                       = st
      override def financialMetrics: FinancialMetricsService[F] = fm
