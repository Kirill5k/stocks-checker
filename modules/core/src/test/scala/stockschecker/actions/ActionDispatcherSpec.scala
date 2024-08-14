package stockschecker.actions

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec

class ActionDispatcherSpec extends IOWordSpec {
  "An ActionDispatcher" should {
    "add an action to the queue of dispatched actions" in {
      (for
        ad      <- ActionDispatcher.make[IO]
        _       <- ad.dispatch(Action.FetchLatestStocks)
        _       <- ad.dispatch(Action.FetchLatestStocks)
        actions <- ad.pendingActions.take(2).compile.toList
      yield actions).asserting { a =>
        a mustBe List(Action.FetchLatestStocks, Action.FetchLatestStocks)
      }
    }
  }
}
