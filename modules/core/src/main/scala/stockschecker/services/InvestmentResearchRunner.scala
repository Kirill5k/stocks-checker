package stockschecker.services

import cats.effect.Temporal
import cats.syntax.all.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import stockschecker.common.config.InvestmentConfig
import scala.concurrent.duration.*

object InvestmentResearchRunner {
  def run[F[_]: {Temporal, Logger}](service: InvestmentScreenService[F], config: InvestmentConfig): Stream[F, Unit] =
    if (!config.enabled || !config.coverageAudited) Stream.never[F]
    else {
      val cycle = (service.generateScheduledScreen >> service.captureMonthlyEvaluations).void
        .handleErrorWith(error => Logger[F].error(error)("Investment research refresh failed; retained published results"))
      Stream.eval(cycle) ++ Stream.awakeEvery[F](1.hour).evalMap(_ => cycle)
    }
}
