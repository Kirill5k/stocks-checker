package stockschecker.repositories

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.kernel.Concurrent
import kirill5k.common.cats.Clock
import mongo4cats.database.MongoDatabase

trait Repositories[F[_]]:
  def security: SecurityRepository[F]
  def companyProfile: CompanyProfileRepository[F]
  def pricePerformanceSummary: PricePerformanceSummaryRepository[F]
  def command: CommandRepository[F]
  def stock: StockRepository[F]

object Repositories:
  def make[F[_]: Clock](db: MongoDatabase[F])(using F: Concurrent[F]): F[Repositories[F]] =
    for
      s  <- SecurityRepository.make(db)
      cp <- CompanyProfileRepository.make(db)
      pps <- PricePerformanceSummaryRepository.make(db)
      c  <- CommandRepository.make(db)
      st <- StockRepository.make(db)
    yield new Repositories[F]:
      override def security: SecurityRepository[F]                 = s
      override def companyProfile: CompanyProfileRepository[F] = cp
      override def pricePerformanceSummary: PricePerformanceSummaryRepository[F] = pps
      override def command: CommandRepository[F]               = c
      override def stock: StockRepository[F]                   = st
