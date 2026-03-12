package stockschecker.actions

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.domain.Ticker
import stockschecker.services.{SecurityService, Services}
import stockschecker.fixtures.*

import java.time.Instant
import scala.concurrent.duration.*

class ActionExecutorSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]
  given Clock[IO]  = Clock.mock(Instant.now())

  "An ActionExecutor" when {
    "handling RecordCompanyProfileUpdate action" should {
      "call security service markAsEnriched method" in {
        val securityService = mock[SecurityService[IO]]
        val services        = mock[Services[IO]]
        val tickers         = List(AAPL, MSFT)

        when(services.security).thenReturn(securityService)
        when(securityService.recordCompanyProfileUpdate(anyList[Ticker])).thenReturnUnit

        (for
          dispatcher <- ActionDispatcher.make[IO]
          executor   <- ActionExecutor.make(dispatcher, services)
          _          <- dispatcher.dispatch(Action.RecordCompanyProfileUpdate(tickers))
          _          <- executor.run.take(1).compile.drain
        yield ()).asserting { _ =>
          verify(securityService).recordCompanyProfileUpdate(tickers)
          succeed
        }
      }

      "handle empty list of tickers" in {
        val securityService = mock[SecurityService[IO]]
        val services        = mock[Services[IO]]

        when(services.security).thenReturn(securityService)
        when(securityService.recordCompanyProfileUpdate(anyList[Ticker])).thenReturnUnit

        (for
          dispatcher <- ActionDispatcher.make[IO]
          executor   <- ActionExecutor.make(dispatcher, services)
          _          <- dispatcher.dispatch(Action.RecordCompanyProfileUpdate(List.empty))
          _          <- executor.run.take(1).compile.drain
        yield ()).asserting { _ =>
          verify(securityService).recordCompanyProfileUpdate(List.empty)
          succeed
        }
      }
    }

    "when action fails" should {
      "queue a Retried action on first failure" in {
        val securityService = mock[SecurityService[IO]]
        val services        = mock[Services[IO]]
        val tickers         = List(AAPL, MSFT)

        when(services.security).thenReturn(securityService)
        when(securityService.recordCompanyProfileUpdate(anyList[Ticker])).thenRaiseError(new RuntimeException("transient error"))

        (for
          dispatcher <- ActionDispatcher.make[IO]
          executor   <- ActionExecutor.make(dispatcher, services)
          _          <- dispatcher.dispatch(Action.RecordCompanyProfileUpdate(tickers))
          _          <- executor.run.take(1).compile.drain
          retried    <- dispatcher.pendingActions.head.compile.lastOrError
        yield retried).asserting { retried =>
          verify(securityService).recordCompanyProfileUpdate(tickers)
          assert(retried == Action.Retried(Action.RecordCompanyProfileUpdate(tickers), 1))
        }
      }

      "execute original action after sleeping when processing a Retried action" in {
        val securityService = mock[SecurityService[IO]]
        val services        = mock[Services[IO]]
        val tickers         = List(AAPL, MSFT)

        when(services.security).thenReturn(securityService)
        when(securityService.recordCompanyProfileUpdate(anyList[Ticker])).thenReturnUnit

        (for
          dispatcher <- ActionDispatcher.make[IO]
          executor   <- ActionExecutor.make(dispatcher, services)
          _          <- dispatcher.dispatch(Action.Retried(Action.RecordCompanyProfileUpdate(tickers), 1))
          _          <- executor.run.take(1).compile.drain
        yield ()).asserting { _ =>
          verify(securityService).recordCompanyProfileUpdate(tickers)
          succeed
        }
      }

      "stop retrying after MaxRetries and not queue another Retried action" in {
        val securityService = mock[SecurityService[IO]]
        val services        = mock[Services[IO]]
        val tickers         = List(AAPL, MSFT)

        when(services.security).thenReturn(securityService)
        when(securityService.recordCompanyProfileUpdate(anyList[Ticker])).thenRaiseError(new RuntimeException("persistent error"))

        (for
          dispatcher <- ActionDispatcher.make[IO]
          executor   <- ActionExecutor.make(dispatcher, services)
          _          <- dispatcher.dispatch(Action.Retried(Action.RecordCompanyProfileUpdate(tickers), 3))
          _          <- executor.run.take(1).compile.drain
          pending    <- dispatcher.pendingActions.head.interruptAfter(200.millis).compile.toList
        yield pending).asserting { pending =>
          verify(securityService).recordCompanyProfileUpdate(tickers)
          pending mustBe empty
        }
      }
    }
  }
}
