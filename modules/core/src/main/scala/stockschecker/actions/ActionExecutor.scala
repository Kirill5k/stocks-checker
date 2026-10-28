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

import java.util.concurrent.ThreadLocalRandom

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
              .handleErrorWith(error => logger.error(s"Job retention failed: ${AppError.safeSummary(error)}"))
          )
          .metered(RetentionInterval)
      )

  private def worker: Stream[F, Unit] = Stream.repeatEval {
    clock.now
      .flatMap(jobs.claim(_, Lease))
      .flatMap {
        case None      => F.sleep(IdlePollInterval).as(Option.empty[Unit])
        case Some(job) => F.race(process(job), heartbeat(job)).as(Some(()))
      }
      .handleErrorWith(error => logger.error(s"Job worker failed: ${AppError.safeSummary(error)}") >> F.sleep(WorkerRecoveryDelay).as(None))
  }.unNone

  private def heartbeat(job: Job): F[Unit] =
    F.sleep(HeartbeatInterval) >> clock.now.flatMap(jobs.renew(job, _, Lease)).flatMap {
      case true  => heartbeat(job)
      case false => F.unit // Losing the lease cancels the competing execution fiber.
    }

  private def process(job: Job): F[Unit] = {
    val execute = for {
      current <- job.command.fold(F.pure(true))(isCurrent)
      _       <-
        if (!current) jobs.finish(job, Job.Cancelled, Some("Command changed or deactivated"))
        else checkDependencies(job)
    } yield ()
    execute.handleErrorWith(handleFailure(job, _))
  }

  private def checkDependencies(job: Job): F[Unit] =
    jobs.summarize(job.dependsOn).flatMap { summary =>
      JobExecutionPolicy.dependencies(job.dependsOn.size, summary) match
        case JobExecutionPolicy.Progress.Finish(state, error) => jobs.finish(job, state, error)
        case JobExecutionPolicy.Progress.Wait                 =>
          clock.now.flatMap(now => jobs.defer(job, now.plusSeconds(pollDelaySeconds(job)), None, countRetry = false))
        case JobExecutionPolicy.Progress.Ready =>
          if job.children.nonEmpty then reconcileChildren(job)
          else job.plan.fold(execute(job))(plan => expand(job, plan.actions, plan.sequential))
    }

  private def reconcileChildren(job: Job): F[Unit] =
    jobs.summarize(job.children).flatMap { summary =>
      JobExecutionPolicy.children(job.children.size, summary) match
        case JobExecutionPolicy.Progress.Finish(state, error) => jobs.finish(job, state, error)
        case JobExecutionPolicy.Progress.Wait                 =>
          clock.now.flatMap(now => jobs.waitForChildren(job, job.children, now.plusSeconds(pollDelaySeconds(job))))
        case JobExecutionPolicy.Progress.Ready => F.unit
    }

  private def expand(job: Job, actions: List[Action], sequential: Boolean): F[Unit] =
    if (actions.isEmpty) jobs.finish(job, Job.Succeeded)
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
      case ActionResult.Completed         => jobs.finish(job, Job.Succeeded)
      case ActionResult.Expand(plan)      => expand(job, plan.actions, plan.sequential)
      case ActionResult.Cancelled(reason) => jobs.finish(job, Job.Cancelled, Some(reason))
    }

  private def handleFailure(job: Job, error: Throwable): F[Unit] =
    val summary = AppError.safeSummary(error)
    JobExecutionPolicy.failure(error, job.retries) match {
      case JobExecutionPolicy.Failure.Defer(retryAt) =>
        for
          now    <- clock.now
          jitter <- F.delay(ThreadLocalRandom.current().nextLong(MaximumProviderDeferJitterMillis + 1))
          until = JobExecutionPolicy.providerDeferUntil(job, now, retryAt, jitter)
          _ <- jobs.defer(job, until, Some(summary), countRetry = false)
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
        logger.warn(s"Job ${job.id} failed: $summary") >> jobs.finish(job, Job.Failed, Some(summary))
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
