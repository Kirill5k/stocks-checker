package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{CommandId, CreateCommand, Schedule}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.{CommandRepository, JobRepository}

import scala.concurrent.duration.*

class CommandServiceSpec extends IOWordSpec {
  given Clock[IO] = Clock.mock(ts)

  "A CommandService" should {
    "initialize a durable schedule and return the updated command" in {
      val (dispatcher, commands, jobs) = mocks
      val input                        = CreateCommand(FetchLatestSecuritiesCommand.action, Schedule.Periodic(1.hour), None)
      val created                      = FetchLatestSecuritiesCommand.copy(lastExecutedAt = None, nextRunAt = None)
      val initialized                  = created.copy(nextRunAt = Some(ts))
      when(commands.create(input)).thenReturnIO(created)
      when(commands.initializeNextRun(created, ts)).thenReturnIO(Some(initialized))
      (for {
        service <- CommandService.make(commands, dispatcher, jobs)
        command <- service.create(input)
      } yield command).asserting { result =>
        result mustBe initialized
        verifyNoInteractions(dispatcher)
        succeed
      }
    }

    "reject an invalid schedule before persisting it" in {
      val (dispatcher, commands, jobs) = mocks
      val input                        = CreateCommand(FetchLatestSecuritiesCommand.action, Schedule.Periodic(0.seconds), None)
      (for {
        service <- CommandService.make(commands, dispatcher, jobs)
        result  <- service.create(input).attempt
      } yield result).asserting { result =>
        result.left.toOption.get mustBe a[AppError.FailedValidation]
        verifyNoInteractions(commands)
        succeed
      }
    }

    "reject retired actions inside newly submitted sequences" in {
      val (dispatcher, commands, jobs) = mocks
      val input                        = CreateCommand(
        Action.Sequence(cats.data.NonEmptyList.of(FetchLatestSecuritiesCommand.action, Action.LegacyRetired)),
        Schedule.Periodic(1.hour),
        None
      )
      (for {
        service <- CommandService.make(commands, dispatcher, jobs)
        result  <- service.create(input).attempt
      } yield result).asserting { result =>
        result.left.toOption.get mustBe a[AppError.FailedValidation]
        verifyNoInteractions(commands, dispatcher, jobs)
        succeed
      }
    }

    "dispatch manual execution independently of a schedule's execution limit" in {
      val (dispatcher, commands, jobs) = mocks
      val command                      = FetchLatestSecuritiesCommand.copy(executionCount = 1, maxExecutions = Some(1))
      when(commands.find(any[CommandId])).thenReturnIO(command)
      when(dispatcher.dispatch(any[Action])).thenReturnUnit
      (for {
        service <- CommandService.make(commands, dispatcher, jobs)
        _       <- service.executeManually(command.id)
      } yield ()).asserting { _ =>
        verify(dispatcher).dispatch(command.action)
        verify(commands).find(command.id)
        verifyNoMoreInteractions(commands)
        succeed
      }
    }

    "cancel obsolete jobs using the revision returned by activation" in {
      val (dispatcher, commands, jobs) = mocks
      val command                      = FetchLatestSecuritiesCommand.copy(isActive = false, version = Some(3L))
      when(commands.setActive(command.id, false)).thenReturnIO(command)
      when(jobs.cancelObsolete(command.id, 3L)).thenReturnUnit
      when(commands.initializeNextRun(command, ts)).thenReturnIO(Some(command))
      (for {
        service <- CommandService.make(commands, dispatcher, jobs)
        _       <- service.activate(command.id, false)
      } yield ()).asserting { _ =>
        verify(jobs).cancelObsolete(command.id, 3L)
        verify(commands).setActive(command.id, false)
        verify(commands).initializeNextRun(command, ts)
        verifyNoMoreInteractions(commands)
        succeed
      }
    }
  }

  private def mocks: (ActionDispatcher[IO], CommandRepository[IO], JobRepository[IO]) =
    (mock[ActionDispatcher[IO]], mock[CommandRepository[IO]], mock[JobRepository[IO]])
}
