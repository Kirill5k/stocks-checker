package stockschecker.actions

import cats.Monad
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import stockschecker.domain.{Job, JobId, Ticker}
import stockschecker.repositories.JobRepository
import stockschecker.services.PriceHistoryService

import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDate}
import java.util.UUID
import scala.concurrent.duration.*

trait EvaluationPriceJobs[F[_]]:
  def pending(ticker: Ticker, through: LocalDate): F[Option[JobId]]
  def ensure(ticker: Ticker, through: LocalDate, now: Instant): F[Option[JobId]]

final private class LiveEvaluationPriceJobs[F[_]: Monad](jobs: JobRepository[F]) extends EvaluationPriceJobs[F]:
  override def pending(ticker: Ticker, through: LocalDate): F[Option[JobId]] =
    val id = EvaluationPriceJobs.id(ticker, through)
    jobs.find(id).map(_.filterNot(_.isTerminal).map(_ => id))

  override def ensure(ticker: Ticker, through: LocalDate, now: Instant): F[Option[JobId]] =
    val id = EvaluationPriceJobs.id(ticker, through)
    // All screens share a ticker/cutoff job and its bounded repair budget.
    jobs.find(id).flatMap {
      case Some(job) if EvaluationPriceJobs.canRestart(job) =>
        val availableAt = job.completedAt.fold(now)(_.plusSeconds(EvaluationPriceJobs.RestartCooldown.toSeconds))
        jobs.restart(job, if availableAt.isAfter(now) then availableAt else now).flatMap {
          case true  => Some(id).pure[F]
          case false => pending(ticker, through)
        }
      case Some(job) if job.isTerminal => pending(ticker, through)
      case Some(_)                     => Some(id).pure[F]
      case None                        =>
        val action = Action.RefreshEvaluationPrices(ticker, through.minus(PriceHistoryService.SharedHistoryLookback), through)
        jobs.enqueue(Job(id, action, now, now), ActionPriority.forAction(action)).as(Some(id))
    }

object EvaluationPriceJobs:
  val MaxRestarts                      = 2
  private[actions] val RestartCooldown = 1.day

  private[actions] def canRestart(job: Job): Boolean =
    job.isTerminal && job.children.isEmpty && job.plan.isEmpty && !job.id.value.contains("/") &&
      job.action.isInstanceOf[Action.RefreshEvaluationPrices] && job.restartCount.getOrElse(0) < MaxRestarts

  private[actions] def id(ticker: Ticker, through: LocalDate): JobId =
    val key = s"evaluation:${ticker.value}:${through.minus(PriceHistoryService.SharedHistoryLookback)}:$through"
    JobId(UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8)).toString)

  def make[F[_]: Monad](jobs: JobRepository[F]): F[EvaluationPriceJobs[F]] =
    Monad[F].pure(LiveEvaluationPriceJobs(jobs))
