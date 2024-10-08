package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import kirill5k.common.syntax.time.*
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.fixtures.*
import stockschecker.repositories.CommandRepository
import fs2.Stream
import stockschecker.actions.Action.Schedule
import stockschecker.domain.{Command, CommandId, CreateCommand}

import scala.concurrent.duration.*

class CommandServiceSpec extends IOWordSpec {

  val now = ts.plus(10.minutes)

  given Clock[IO] = Clock.mock(now)

  "A CommandService" when {
    "create" should {
      "create new command and schedule it for execution" in {
        val cc     = CreateCommand(FetchLatestStocksCommand.action, FetchLatestStocksCommand.schedule, None)
        val newCmd = FetchLatestStocksCommand.copy(lastExecutedAt = None)

        val (ad, repo) = mocks
        when(repo.create(any[CreateCommand])).thenReturnIO(newCmd)
        when(ad.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CommandService.make(repo, ad)
          cmd <- svc.create(cc)
        yield cmd

        res.asserting { r =>
          verify(repo).create(cc)
          verify(ad).dispatch(Action.Schedule(FetchLatestStocksCommand.id, 0.minutes))
          r mustBe newCmd
        }
      }
    }

    "rescheduleAll" should {
      "not do anything when there are no active commands" in {
        val (ad, repo) = mocks
        when(repo.streamActive).thenReturn(Stream.empty)

        val res = for
          svc <- CommandService.make(repo, ad)
          _   <- svc.rescheduleAll
        yield ()

        res.asserting { r =>
          verifyNoInteractions(ad)
          verify(repo).streamActive
          r mustBe ()
        }
      }

      "reschedule command that needs to be executed" in {
        val (ad, repo) = mocks
        when(repo.streamActive).thenStream(FetchLatestStocksCommand)
        when(ad.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CommandService.make(repo, ad)
          _   <- svc.rescheduleAll
        yield ()

        res.asserting { r =>
          verify(repo).streamActive
          verify(ad).dispatch(Action.Schedule(FetchLatestStocksCommand.id, 10.minutes))
          r mustBe ()
        }
      }
    }

    "execute" should {
      "dispatch an action from command" in {
        val (ad, repo) = mocks
        when(repo.find(any[CommandId])).thenReturnIO(FetchLatestStocksCommand)
        when(repo.update(any[Command])).thenReturnIO(FetchLatestStocksCommand)
        when(ad.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CommandService.make(repo, ad)
          _   <- svc.execute(FetchLatestStocksCommand.id)
        yield ()

        res.asserting { r =>
          verify(repo).find(FetchLatestStocksCommand.id)
          verify(ad).dispatch(FetchLatestStocksCommand.action)
          verify(repo).update(FetchLatestStocksCommand.copy(lastExecutedAt = Some(now), executionCount = 2))
          verify(ad).dispatch(Action.Schedule(FetchLatestStocksCommand.id, 20.minutes))
          r mustBe ()
        }
      }

      "not execute command when maxExecutions is equal to executionCount" in {
        val (ad, repo) = mocks
        when(repo.find(any[CommandId])).thenReturnIO(FetchLatestStocksCommand.copy(executionCount = 2, maxExecutions = Some(2)))
        when(ad.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CommandService.make(repo, ad)
          _   <- svc.execute(FetchLatestStocksCommand.id)
        yield ()

        res.asserting { r =>
          verify(repo).find(FetchLatestStocksCommand.id)
          verify(ad).dispatch(Action.Schedule(FetchLatestStocksCommand.id, 20.minutes))
          verifyNoMoreInteractions(repo)
          r mustBe ()
        }
      }

      "not execute command when is it inactive" in {
        val (ad, repo) = mocks
        when(repo.find(any[CommandId])).thenReturnIO(FetchLatestStocksCommand.copy(isActive = false))
        when(ad.dispatch(any[Action])).thenReturnUnit

        val res = for
          svc <- CommandService.make(repo, ad)
          _   <- svc.execute(FetchLatestStocksCommand.id)
        yield ()

        res.asserting { r =>
          verify(repo).find(FetchLatestStocksCommand.id)
          verify(ad).dispatch(Action.Schedule(FetchLatestStocksCommand.id, 20.minutes))
          verifyNoMoreInteractions(repo)
          r mustBe ()
        }
      }
    }
  }

  def mocks: (ActionDispatcher[IO], CommandRepository[IO]) =
    mock[ActionDispatcher[IO]] -> mock[CommandRepository[IO]]
}
