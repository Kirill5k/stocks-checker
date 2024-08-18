package stockschecker.repositories

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.kernel.Concurrent
import mongo4cats.database.MongoDatabase

trait Repositories[F[_]]:
  def stock: StockRepository[F]
  def companyProfile: CompanyProfileRepository[F]
  def command: CommandRepository[F]

object Repositories:
  def make[F[_]](db: MongoDatabase[F])(using F: Concurrent[F]): F[Repositories[F]] =
    for
      s <- StockRepository.make(db)
      cp <- CompanyProfileRepository.make(db)
      c <- CommandRepository.make(db)
    yield new Repositories[F]:
      override def stock: StockRepository[F] = s
      override def companyProfile: CompanyProfileRepository[F] = cp
      override def command: CommandRepository[F] = c
