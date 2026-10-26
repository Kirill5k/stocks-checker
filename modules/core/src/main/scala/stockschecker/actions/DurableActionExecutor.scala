package stockschecker.actions

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import stockschecker.common.config.ActionExecutorConfig
import stockschecker.domain.{Job, JobPlan}
import stockschecker.domain.errors.AppError
import stockschecker.repositories.JobRepository
import stockschecker.services.Services

import java.util.concurrent.{ThreadLocalRandom, TimeoutException}
import scala.concurrent.duration.*

private[actions] final class DurableActionExecutor[F[_]](
    jobs: JobRepository[F], services: Services[F], config: ActionExecutorConfig
)(using F: Async[F], clock: Clock[F], logger: Logger[F]) extends ActionExecutor[F] {
  private val lease = 2.minutes

  override def run: Stream[F, Unit] =
    Stream.emits(List.fill(config.concurrency.max(1))(worker)).parJoin(config.concurrency.max(1))

  private def worker: Stream[F, Unit] = Stream.repeatEval {
    clock.now.flatMap(jobs.claim(_, lease)).flatMap {
      case None => F.sleep(250.millis).as(Option.empty[Unit])
      case Some(job) => F.race(process(job), heartbeat(job)).as(Some(()))
    }.handleErrorWith(error => logger.error(s"Job worker failed: ${summary(error)}") >> F.sleep(1.second).as(None))
  }.unNone

  private def heartbeat(job: Job): F[Unit] =
    F.sleep(20.seconds) >> clock.now.flatMap(jobs.renew(job, _, lease)).flatMap {
      case true => heartbeat(job)
      case false => F.unit // Losing the lease cancels the competing execution fiber.
    }

  private def process(job: Job): F[Unit] = {
    val execute = for {
      current <- job.command.fold(F.pure(true))(services.command.isCurrent)
      _ <- if (!current) jobs.finish(job, Job.Cancelled, Some("Command changed or deactivated"))
           else checkDependencies(job)
    } yield ()
    execute.handleErrorWith(handleFailure(job, _))
  }

  private def checkDependencies(job: Job): F[Unit] =
    job.dependsOn.traverse(jobs.find).flatMap { dependencies =>
      if (dependencies.flatten.exists(j => j.isTerminal && !j.hasUsableResults))
        jobs.finish(job, Job.Cancelled, Some("A preceding step did not succeed"))
      else if (dependencies.exists(_.forall(!_.hasUsableResults)))
        clock.now.flatMap(now => jobs.defer(job, now.plusSeconds(1), None, countRetry = false))
      else if (job.children.nonEmpty) reconcileChildren(job)
      else if (job.plan.nonEmpty) expand(job, job.plan.get.actions, job.plan.get.sequential)
      else execute(job, job.action)
    }

  private def reconcileChildren(job: Job): F[Unit] =
    job.children.traverse(jobs.find).flatMap { children =>
      if (children.exists(_.forall(!_.isTerminal)))
        clock.now.flatMap(jobs.waitForChildren(job, job.children, _))
      else if (children.flatten.exists(j => j.state == Job.Failed || j.state == Job.Partial))
        jobs.finish(job, if (children.flatten.exists(_.hasUsableResults)) Job.Partial else Job.Failed,
          Some("One or more child jobs failed; successful results were retained"))
      else if (children.flatten.exists(_.state == Job.Cancelled))
        jobs.finish(job, Job.Cancelled, Some("One or more child jobs were cancelled"))
      else jobs.finish(job, Job.Succeeded)
    }

  private def expand(job: Job, actions: List[Action], sequential: Boolean = false): F[Unit] =
    if (actions.isEmpty) jobs.finish(job, Job.Succeeded)
    else clock.now.flatMap { now =>
      val children = actions.zipWithIndex.map { case (action, index) =>
        val dependencies = if (sequential && index > 0) List(s"${job.id}/${index - 1}") else Nil
        val due = action match {
          case Action.Schedule(_, duration) => now.plusMillis(duration.toMillis)
          case _ => now
        }
        Job(s"${job.id}/$index", action, due, now, job.command, dependencies)
      }
      // Freeze a query's result before materialization: a crash/retry must not map the same child IDs to a different universe.
      (if (job.plan.nonEmpty) F.pure(true) else jobs.savePlan(job, JobPlan(actions, sequential))).flatMap { owned =>
        F.whenA(owned)(children.traverse_(jobs.enqueue) >> jobs.waitForChildren(job, children.map(_.id), now))
      }
    }

  private def done(job: Job)(action: F[Unit]): F[Unit] = action >> jobs.finish(job, Job.Succeeded)

  private def execute(job: Job, action: Action): F[Unit] = action match {
    case Action.Sequence(actions) => expand(job, actions.toList, sequential = true)
    case Action.RescheduleAll => done(job)(services.command.rescheduleAll)
    case Action.Schedule(id, _) => done(job)(services.command.execute(id))
    case Action.Retried(original, _) => execute(job, original)
    case Action.FetchSecurities(exchanges) => done(job)(services.security.fetchLatest(exchanges))
    case Action.FetchCompanyProfiles(filter, limit) =>
      services.security.findTickersBy(filter, limit).flatMap(tickers => expand(job, tickers.map(t => Action.UpdateCompanyProfiles(NonEmptyList.one(t)))))
    case Action.FetchPriceAnalytics(filter, limit) =>
      services.companyProfile.findTickersBy(filter, limit).flatMap(tickers => expand(job, tickers.map(t => Action.UpdatePriceAnalytics(NonEmptyList.one(t)))))
    case Action.FetchFinancialMetrics(filter, limit) =>
      services.companyProfile.findTickersBy(filter, limit).flatMap(tickers => expand(job, tickers.map(t => Action.UpdateFinancialMetrics(NonEmptyList.one(t)))))
    case Action.UpdateCompanyProfiles(tickers) if tickers.size == 1 => done(job)(services.companyProfile.get(tickers.head, true).void)
    case Action.UpdatePriceAnalytics(tickers) if tickers.size == 1 => done(job)(services.price.findPriceAnalytics(tickers.head, true).void)
    case Action.UpdateFinancialMetrics(tickers) if tickers.size == 1 => done(job)(services.financialMetrics.find(tickers.head, true).void)
    case Action.UpdateCompanyProfiles(tickers) => expand(job, tickers.toList.map(t => Action.UpdateCompanyProfiles(NonEmptyList.one(t))))
    case Action.UpdatePriceAnalytics(tickers) => expand(job, tickers.toList.map(t => Action.UpdatePriceAnalytics(NonEmptyList.one(t))))
    case Action.UpdateFinancialMetrics(tickers) => expand(job, tickers.toList.map(t => Action.UpdateFinancialMetrics(NonEmptyList.one(t))))
    case Action.RefreshEvaluationPrices(ticker, from, through) => done(job)(services.investmentScreens.refreshHistory(ticker, from, through))
    case Action.RecordCompanyProfileUpdate(tickers) => done(job)(services.security.recordCompanyProfileUpdate(tickers))
    case Action.RecordPriceAnalyticsUpdate(tickers) => done(job)(services.companyProfile.recordPriceAnalyticsUpdate(tickers))
    case Action.RecordFinancialMetricsUpdate(tickers) => done(job)(services.companyProfile.recordFinancialMetricsUpdate(tickers))
    case Action.DeactivateSecurity(ticker) => done(job)(services.security.deactivate(ticker))
    case Action.DeactivateCompanyProfile(ticker) => done(job)(services.companyProfile.deactivate(ticker))
  }

  private def handleFailure(job: Job, error: Throwable): F[Unit] =
    logger.warn(s"Job ${job.id} failed: ${summary(error)}") >> (error match {
      case AppError.ProviderDeferred(_, retryAt, _) => jobs.defer(job, retryAt, Some(summary(error)), countRetry = false)
      case AppError.ProviderConfiguration(provider, _) => jobs.pause(job, provider, summary(error))
      case _ if transient(error) && job.retries < 3 =>
        for {
          now <- clock.now
          jitter <- F.delay(ThreadLocalRandom.current().nextLong(500L, 1501L))
          _ <- jobs.defer(job, now.plusMillis((1000L << job.retries) + jitter), Some(summary(error)), countRetry = true)
        } yield ()
      case _ => jobs.finish(job, Job.Failed, Some(summary(error)))
    })

  private def transient(error: Throwable): Boolean = error match {
    case AppError.HttpClient(_, status, _) => status == 408 || status == 429 || status >= 500
    case _: java.io.IOException | _: TimeoutException | _: com.mongodb.MongoException | _: sttp.client4.SttpClientException => true
    case _ => false
  }

  // Provider response bodies and request URLs can contain credentials; never persist them in job outcomes.
  private def summary(error: Throwable): String = error match {
    case AppError.HttpClient(provider, status, _) => s"$provider HTTP $status"
    case AppError.ProviderDeferred(provider, _, _) => s"$provider rate limit/cooldown"
    case AppError.ProviderConfiguration(provider, _) => s"$provider configuration requires attention"
    case _: AppError.JsonParsingFailure => "Invalid provider response"
    case _ => error.getClass.getSimpleName
  }
}
