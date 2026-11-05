package stockschecker.services

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.traverse.*
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.domain.{CommandId, CreateCommand, JobId, Schedule, UpdateCommand}
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
      val jobId                  = JobId("manual-run")
      when(commands.find(any[CommandId])).thenReturnIO(command)
      when(dispatcher.dispatchForCommand(command.id, command.action)).thenReturnIO(jobId)
      (for {
        service <- CommandService.make(commands, dispatcher)
        result  <- service.executeManually(command.id)
      } yield result).asserting { result =>
        result mustBe jobId
        verify(dispatcher).dispatchForCommand(command.id, command.action)
        verify(commands).find(command.id)
        verifyNoMoreInteractions(commands)
        succeed
      }
    }

    "delete a command once before deleting its jobs directly" in {
      val (dispatcher, commands) = mocks
      val id                     = FetchLatestSecuritiesCommand.id
      when(dispatcher.deleteForCommand(id)).thenReturnUnit
      when(commands.delete(id)).thenReturnUnit
      (for
        service <- CommandService.make(commands, dispatcher)
        _       <- service.delete(id)
      yield ()).asserting { _ =>
        val order = org.mockito.Mockito.inOrder(dispatcher, commands)
        order.verify(commands).delete(id)
        order.verify(dispatcher).deleteForCommand(id)
        verifyNoMoreInteractions(commands, dispatcher)
        succeed
      }
    }

    "report job cleanup failure after deleting the command" in {
      val (dispatcher, commands) = mocks
      val id                     = FetchLatestSecuritiesCommand.id
      val failure                = new java.io.IOException("job storage unavailable")
      when(commands.delete(id)).thenReturnUnit
      when(dispatcher.deleteForCommand(id)).thenRaiseError(failure)
      CommandService.make(commands, dispatcher).flatMap(_.delete(id)).attempt.asserting { result =>
        result mustBe Left(failure)
        val order = org.mockito.Mockito.inOrder(commands, dispatcher)
        order.verify(commands).delete(id)
        order.verify(dispatcher).deleteForCommand(id)
        verifyNoMoreInteractions(commands, dispatcher)
        succeed
      }
    }

    "not delete jobs when deleting the command fails" in {
      val (dispatcher, commands) = mocks
      val id                     = FetchLatestSecuritiesCommand.id
      val failure                = new java.io.IOException("command storage unavailable")
      when(commands.delete(id)).thenRaiseError(failure)
      CommandService.make(commands, dispatcher).flatMap(_.delete(id)).attempt.asserting { result =>
        result mustBe Left(failure)
        verifyNoInteractions(dispatcher)
        succeed
      }
    }

    "not delete jobs when the command does not exist" in {
      val (dispatcher, commands) = mocks
      val id                     = FetchLatestSecuritiesCommand.id
      val failure                = AppError.EntityDoesNotExist("Command", id.value)
      when(commands.delete(id)).thenRaiseError(failure)
      CommandService.make(commands, dispatcher).flatMap(_.delete(id)).attempt.asserting { result =>
        result mustBe Left(failure)
        verifyNoInteractions(dispatcher)
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
        Action.DeleteCompanyProfile(AAPL),
        Action.SetSecurityActive(AAPL, false, java.time.Instant.EPOCH),
        Action.PurgeStock(AAPL, java.time.Instant.EPOCH),
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
