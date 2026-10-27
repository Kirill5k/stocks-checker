package stockschecker.actions

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import kirill5k.common.cats.Clock
import stockschecker.domain.{Job, JobId}
import stockschecker.fixtures.*
import stockschecker.repositories.{JobRepository, RepositorySpec}

import scala.concurrent.duration.*

class ActionDispatcherIntegrationSpec extends RepositorySpec:
  override def port: Int = 12212
  given Clock[IO] = Clock.mock(ts)

  "A durable dispatcher" should {
    "reuse one accepted job across concurrent producers and repository recreation without restarting finished work" in withEmbeddedMongoDatabase { db =>
      val id = JobId("shared-capture")
      val action = Action.DeactivateSecurity(AAPL)
      for
        repository <- JobRepository.make[IO](db)
        first <- ActionDispatcher.make(repository)
        _ <- (first.dispatchOnce(id, action), first.dispatchOnce(id, action)).parTupled
        pending <- first.pending(List(id))
        claimed <- repository.claim(ts, 2.minutes).map(_.get)
        _ <- repository.finish(claimed, Job.Failed)
        reopened <- JobRepository.make[IO](db)
        second <- ActionDispatcher.make(reopened)
        _ <- second.dispatchOnce(id, Action.DeactivateSecurity(MSFT))
        remaining <- second.pending(List(id))
        saved <- reopened.find(id).map(_.get)
        duplicate <- reopened.claim(ts, 2.minutes)
      yield
        pending mustBe Set(id)
        remaining mustBe empty
        saved.action mustBe action
        saved.state mustBe Job.Failed
        saved.createdAt mustBe ts
        duplicate mustBe None
    }

    "return only existing unfinished jobs, including paused jobs" in withEmbeddedMongoDatabase { db =>
      val states = List(Job.Pending, Job.Running, Job.Waiting, Job.Paused, Job.Succeeded, Job.Failed, Job.Partial, Job.Cancelled)
      for
        repository <- JobRepository.make[IO](db)
        _ <- states.traverse_(state =>
          repository.enqueue(Job(JobId(state.value), Action.DeactivateSecurity(AAPL), ts, ts, state = state), ActionPriority.Normal)
        )
        dispatcher <- ActionDispatcher.make(repository)
        pending <- dispatcher.pending(JobId("missing") :: states.map(state => JobId(state.value)))
        noIds <- dispatcher.pending(Nil)
      yield
        pending mustBe Set(Job.Pending, Job.Running, Job.Waiting, Job.Paused).map(state => JobId(state.value))
        noIds mustBe empty
    }
  }
