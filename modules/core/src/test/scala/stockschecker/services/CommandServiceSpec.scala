package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{CommandId, CreateCommand, Schedule, UpdateCommand}
import stockschecker.domain.errors.AppError
import stockschecker.fixtures.*
import stockschecker.repositories.CommandRepository

import scala.concurrent.duration.*

class CommandServiceSpec extends IOWordSpec {
  given Clock[IO] = Clock.mock(ts)

  "A CommandService" should {
    "initialize a durable schedule and return the updated command" in {
      val (dispatcher, commands) = mocks
      val input                  = CreateCommand(FetchLatestSecuritiesCommand.action, Schedule.Periodic(1.hour), None)
      val created                = FetchLatestSecuritiesCommand.copy(lastExecutedAt = None, nextRunAt = None)
      val initialized            = created.copy(nextRunAt = Some(ts))
      when(commands.create(input)).thenReturnIO(created)
      when(commands.initializeNextRun(created, ts)).thenReturnIO(Some(initialized))
      (for {
        service <- CommandService.make(commands, dispatcher)
        command <- service.create(input)
      } yield command).asserting { result =>
        result mustBe initialized
        verifyNoInteractions(dispatcher)
        succeed
      }
    }

    "reject an invalid schedule before persisting it" in {
      val (dispatcher, commands) = mocks
      val input                  = CreateCommand(FetchLatestSecuritiesCommand.action, Schedule.Periodic(0.seconds), None)
      (for {
        service <- CommandService.make(commands, dispatcher)
        result  <- service.create(input).attempt
      } yield result).asserting { result =>
        result.left.toOption.get mustBe a[AppError.FailedValidation]
        verifyNoInteractions(commands)
        succeed
      }
    }

    "dispatch manual execution independently of a schedule's execution limit" in {
      val (dispatcher, commands) = mocks
      val command                = FetchLatestSecuritiesCommand.copy(executionCount = 1, maxExecutions = Some(1))
      when(commands.find(any[CommandId])).thenReturnIO(command)
      when(dispatcher.dispatch(any[Action])).thenReturnUnit
      (for {
        service <- CommandService.make(commands, dispatcher)
        _       <- service.executeManually(command.id)
      } yield ()).asserting { _ =>
        verify(dispatcher).dispatch(command.action)
        verify(commands).find(command.id)
        verifyNoMoreInteractions(commands)
        succeed
      }
    }

    "reject internal actions and nested internal events before creating or updating commands" in {
      val (dispatcher, commands) = mocks
      val events                 = List(
        Action.RecordCompanyProfileUpdate(NonEmptyList.one(AAPL)),
        Action.RecordPriceAnalyticsUpdate(NonEmptyList.one(AAPL)),
        Action.RecordFinancialMetricsUpdate(NonEmptyList.one(AAPL)),
        Action.SetSecurityActive(AAPL, true, java.time.Instant.EPOCH),
        Action.SetCompanyProfileActive(AAPL, true, java.time.Instant.EPOCH),
        Action.SetSecurityActive(AAPL, false, java.time.Instant.EPOCH),
        Action.SetCompanyProfileActive(AAPL, false, java.time.Instant.EPOCH),
        Action.RefreshEvaluationPrices(AAPL, java.time.LocalDate.parse("2026-02-01")),
        Action.Sequence(
          NonEmptyList.of(
            Action.RefreshInvestmentResearch,
            Action.Sequence(NonEmptyList.one(Action.SetSecurityActive(AAPL, true, java.time.Instant.EPOCH)))
          )
        )
      )
      (for {
        service <- CommandService.make(commands, dispatcher)
        created <- events.traverse(action => service.create(CreateCommand(action, Schedule.Periodic(1.hour), None)).attempt)
        updated <- events.traverse(action =>
          service.update(UpdateCommand(FetchLatestSecuritiesCommand.id, true, action, Schedule.Periodic(1.hour), None)).attempt
        )
      } yield created ++ updated).asserting { results =>
        results.foreach(_.left.toOption.get mustBe a[AppError.FailedValidation])
        verifyNoInteractions(commands, dispatcher)
        succeed
      }
    }

    "cancel obsolete jobs using the revision returned by activation" in {
      val (dispatcher, commands) = mocks
      val command                = FetchLatestSecuritiesCommand.copy(isActive = false, version = 3L)
      when(commands.setActive(command.id, false)).thenReturnIO(command)
      when(dispatcher.cancelObsolete(command.id, 3L)).thenReturnUnit
      when(commands.initializeNextRun(command, ts)).thenReturnIO(Some(command))
      (for {
        service <- CommandService.make(commands, dispatcher)
        _       <- service.activate(command.id, false)
      } yield ()).asserting { _ =>
        verify(dispatcher).cancelObsolete(command.id, 3L)
        verify(commands).setActive(command.id, false)
        verify(commands).initializeNextRun(command, ts)
        verifyNoMoreInteractions(commands)
        succeed
      }
    }
  }

  private def mocks: (ActionDispatcher[IO], CommandRepository[IO]) =
    (mock[ActionDispatcher[IO]], mock[CommandRepository[IO]])
}
