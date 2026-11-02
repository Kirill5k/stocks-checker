package stockschecker.services

import cats.Monad
import cats.syntax.flatMap.*
import kirill5k.common.cats.Clock
import stockschecker.domain.{DataQualityReport, InvestmentRecipe}
import stockschecker.repositories.DataQualityRepository

trait DataQualityService[F[_]]:
  def report: F[DataQualityReport]

final private class LiveDataQualityService[F[_]](
    private val repository: DataQualityRepository[F],
    private val recipe: InvestmentRecipe
)(using
    F: Monad[F],
    C: Clock[F]
) extends DataQualityService[F] {
  override def report: F[DataQualityReport] =
    C.now.flatMap(repository.report(_, recipe))
}

object DataQualityService:
  def make[F[_]: {Monad, Clock}](
      repository: DataQualityRepository[F],
      recipe: InvestmentRecipe
  ): F[DataQualityService[F]] =
    Monad[F].pure(LiveDataQualityService[F](repository, recipe))
