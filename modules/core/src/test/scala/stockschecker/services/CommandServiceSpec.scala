package stockschecker.services

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import kirill5k.common.syntax.time.*
import stockschecker.actions.{Action, ActionDispatcher}
import stockschecker.fixtures.*
import stockschecker.repositories.CommandRepository
import fs2.Stream

import scala.concurrent.duration.*

class CommandServiceSpec extends IOWordSpec {

  given Clock[IO] = Clock.mock(ts.plus(10.minutes))

  "A CommandService" when {
    "rescheduleAll" should {
      "not do anything when there are no active commands" in {
        val (ad, repo) = mocks
        when(repo.streamActive).thenReturn(Stream.empty)

        val res = for
          svc <- CommandService.make(ad, repo)
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
          svc <- CommandService.make(ad, repo)
          _ <- svc.rescheduleAll
        yield ()

        res.asserting { r =>
          verify(repo).streamActive
          verify(ad).dispatch(Action.Schedule(FetchLatestStocksCommand.id, 10.minutes))
          r mustBe()
        }
      }
    }
  }

  def mocks: (ActionDispatcher[IO], CommandRepository[IO]) =
    mock[ActionDispatcher[IO]] -> mock[CommandRepository[IO]]
}
