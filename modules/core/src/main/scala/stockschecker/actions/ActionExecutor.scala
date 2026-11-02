package stockschecker.actions

import cats.effect.Async
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import fs2.Stream
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.{Job, JobCommand, JobId, JobPlan}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.JobRepository

import java.time.Instant
import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.duration.*

trait ActionExecutor[F[_]]:
  def run: Stream[F, Unit]

final private class LiveActionExecutor[F[_]](
    jobs: JobRepository[F],
    handler: ActionHandler[F],
    isCurrent: JobCommand => F[Boolean],
    config: ActionExecutorConfig
)(using F: Async[F], clock: Clock[F], logger: Logger[F])
    extends ActionExecutor[F] {
  import JobExecutionPolicy.*

  override def run: Stream[F, Unit] =
    Stream
      .emits(List.fill(config.concurrency.max(1))(worker))
      .parJoin(config.concurrency.max(1))
      .concurrently(
        Stream
          .repeatEval(
            clock.now
              .flatMap(jobs.pruneCompleted)
              .handleErrorWith(error => logger.error(s"Job retention failed: ${error.getMessage}"))
          )
          .metered(RetentionInterval)
      )

  private def worker: Stream[F, Unit] = Stream.repeatEval {
    clock.now
      .flatMap(jobs.claim(_, Lease))
      .flatMap {
        case None      => F.sleep(IdlePollInterval).as(Option.empty[Unit])
        case Some(job) =>
          clock.now
            .flatMap(now => F.race(process(job), heartbeat(job, job.leaseUntil.getOrElse(now.plusMillis(Lease.toMillis)))))
            .as(Some(()))
      }
      .handleErrorWith(error => logger.error(s"Job worker failed: ${error.getMessage}") >> F.sleep(WorkerRecoveryDelay).as(None))
  }.unNone

  private def heartbeat(job: Job, leaseUntil: Instant, delay: FiniteDuration = HeartbeatInterval): F[Unit] =
    clock.now.flatMap { before =>
      val remaining = java.time.Duration.between(before, leaseUntil).toMillis.millis
      if remaining <= Duration.Zero then F.unit
      else
        F.sleep(delay.min(remaining)) >> clock.now.flatMap { now =>
          if !now.isBefore(leaseUntil) then F.unit
          else
            F.timeoutTo(
              jobs.renew(job, now, Lease).attempt,
              java.time.Duration.between(now, leaseUntil).toMillis.millis,
              F.pure(Right(false))
            ).flatMap {
              case Right(true)  => heartbeat(job, now.plusMillis(Lease.toMillis))
              case Right(false) => F.unit // Losing the lease cancels the competing execution fiber.
              case Left(error)  =>
                logger.error(error)(s"Heartbeat renewal failed for job ${job.id}; retrying while the lease is valid") >>
                  heartbeat(job, leaseUntil, WorkerRecoveryDelay)
            }
        }
    }

  // Completion is a separate durable write: retrying it must never invoke the action again.
  // The heartbeat continues to fence this writer, and lease loss cancels both fibers.
  private def finish(job: Job, state: stockschecker.domain.JobState, error: Option[String] = None): F[Unit] =
    jobs
      .finish(job, state, error)
      .handleErrorWith { failure =>
        logger.error(failure)(s"Completion write failed for job ${job.id}; retrying the write") >>
          F.sleep(WorkerRecoveryDelay) >> finish(job, state, error)
      }

  private def process(job: Job): F[Unit] = {
    val execute = for
      current <- job.command.fold(F.pure(true))(isCurrent)
      _       <-
        if (!current) finish(job, Job.Cancelled, Some("Command changed or deactivated"))
        else checkDependencies(job)
    yield ()
    execute.handleErrorWith(handleFailure(job, _))
  }

  private def checkDependencies(job: Job): F[Unit] =
    jobs.summarize(job.dependsOn).flatMap { summary =>
      JobExecutionPolicy.dependencies(job.dependsOn.size, summary) match
        case JobExecutionPolicy.Progress.Finish(state, error) => finish(job, state, error)
        case JobExecutionPolicy.Progress.Wait                 =>
          clock.now.flatMap(now => jobs.defer(job, now.plusSeconds(pollDelaySeconds(job)), None, countRetry = false))
        case JobExecutionPolicy.Progress.Ready =>
          if job.children.nonEmpty then reconcileChildren(job)
          else job.plan.fold(execute(job))(plan => expand(job, plan.actions, plan.sequential))
    }

  private def reconcileChildren(job: Job): F[Unit] =
    jobs.summarize(job.children).flatMap { summary =>
      JobExecutionPolicy.children(job.children.size, summary) match
        case JobExecutionPolicy.Progress.Finish(state, error) => finish(job, state, error)
        case JobExecutionPolicy.Progress.Wait                 =>
          clock.now.flatMap(now => jobs.waitForChildren(job, job.children, now.plusSeconds(pollDelaySeconds(job))))
        case JobExecutionPolicy.Progress.Ready => F.unit
    }

  private def expand(job: Job, actions: List[Action], sequential: Boolean): F[Unit] =
    if (actions.isEmpty) finish(job, Job.Succeeded)
    else
      clock.now.flatMap { now =>
        val children = actions.zipWithIndex.map { case (action, index) =>
          val dependencies = if (sequential && index > 0) List(JobId(s"${job.id.value}/${index - 1}")) else Nil
          Job(JobId(s"${job.id.value}/$index"), action, now, now, job.command, dependencies)
        }
        // Freeze a query's result before materialization: a crash/retry must not map the same child IDs to a different universe.
        (if (job.plan.nonEmpty) F.pure(true) else jobs.savePlan(job, JobPlan(actions, sequential))).flatMap { owned =>
          F.whenA(owned)(
            children.traverse_(child => jobs.enqueue(child, ActionPriority.forAction(child.action))) >> jobs
              .waitForChildren(job, children.map(_.id), now.plusSeconds(pollDelaySeconds(job)))
          )
        }
      }

  private def execute(job: Job): F[Unit] =
    handler.execute(job.action).flatMap {
      case ActionResult.Completed    => finish(job, Job.Succeeded)
      case ActionResult.Expand(plan) => expand(job, plan.actions, plan.sequential)
    }

  private def handleFailure(job: Job, error: Throwable): F[Unit] =
    val summary = error.getMessage
    JobExecutionPolicy.failure(error, job.retries) match {
      case JobExecutionPolicy.Failure.Defer(retryAt) =>
        for
          now    <- clock.now
          jitter <- F.delay(ThreadLocalRandom.current().nextLong(MaximumProviderDeferJitterMillis + 1))
          until = JobExecutionPolicy.providerDeferUntil(job, now, retryAt, jitter)
          _ <- jobs.defer(job, until, Some(summary), countRetry = false, countDeferral = true)
        yield ()
      case JobExecutionPolicy.Failure.Pause(provider) =>
        logger.warn(s"Job ${job.id} paused: $summary") >> jobs.pause(job, provider, summary)
      case JobExecutionPolicy.Failure.Retry =>
        for {
          _      <- logger.warn(s"Job ${job.id} failed: $summary")
          now    <- clock.now
          jitter <- F.delay(ThreadLocalRandom.current().nextLong(MinimumRetryJitterMillis, MaximumRetryJitterMillis + 1))
          _      <- jobs.defer(
            job,
            now.plusMillis(JobExecutionPolicy.retryDelay(job.retries, jitter).toMillis),
            Some(summary),
            countRetry = true
          )
        } yield ()
      case JobExecutionPolicy.Failure.Fail =>
        val message = s"Job ${job.id} failed: $summary"
        val log     = error match
          case _: AppError.DataUnavailable | _: AppError.CompanyProfileNotFound =>
            logger.info(message)
          case _ => logger.warn(message)
        log >> finish(job, Job.Failed, Some(summary))
    }
}

object ActionExecutor:
  def make[F[_]: {Async, Logger, Clock}](
      jobs: JobRepository[F],
      handler: ActionHandler[F],
      isCurrent: JobCommand => F[Boolean],
      config: ActionExecutorConfig
  ): F[ActionExecutor[F]] =
    Async[F].pure(new LiveActionExecutor(jobs, handler, isCurrent, config))
