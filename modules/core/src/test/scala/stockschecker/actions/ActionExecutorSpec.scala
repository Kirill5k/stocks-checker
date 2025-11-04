package stockschecker.actions

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import stockschecker.domain.Ticker
import stockschecker.services.{SecurityService, Services}
import stockschecker.fixtures.*

class ActionExecutorSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "An ActionExecutor" when {
    "handling MarkSecuritiesAsEnriched action" should {
      "call security service markAsEnriched method" in {
        val securityService = mock[SecurityService[IO]]
        val services        = mock[Services[IO]]
        val tickers         = List(AAPL, MSFT)

        when(services.security).thenReturn(securityService)
        when(securityService.markAsEnriched(anyList[Ticker])).thenReturnUnit

        (for
          dispatcher <- ActionDispatcher.make[IO]
          executor   <- ActionExecutor.make(dispatcher, services)
          _          <- dispatcher.dispatch(Action.MarkSecuritiesAsEnriched(tickers))
          _          <- executor.run.take(1).compile.drain
        yield ()).asserting { _ =>
          verify(securityService).markAsEnriched(tickers)
          succeed
        }
      }

      "handle empty list of tickers" in {
        val securityService = mock[SecurityService[IO]]
        val services        = mock[Services[IO]]

        when(services.security).thenReturn(securityService)
        when(securityService.markAsEnriched(anyList[Ticker])).thenReturnUnit

        (for
          dispatcher <- ActionDispatcher.make[IO]
          executor   <- ActionExecutor.make(dispatcher, services)
          _          <- dispatcher.dispatch(Action.MarkSecuritiesAsEnriched(List.empty))
          _          <- executor.run.take(1).compile.drain
        yield ()).asserting { _ =>
          verify(securityService).markAsEnriched(List.empty)
          succeed
        }
      }
    }
  }
}
