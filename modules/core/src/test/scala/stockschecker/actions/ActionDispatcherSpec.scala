package stockschecker.actions

import cats.data.NonEmptyList
import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import stockschecker.domain.Exchange

class ActionDispatcherSpec extends IOWordSpec {
  "An ActionDispatcher" should {
    "add an action to the queue of dispatched actions" in {
      val a1 = Action.DiscoverSecurities(NonEmptyList.of(Exchange.NYSE))
      val a2 = Action.DiscoverSecurities(NonEmptyList.of(Exchange.NASDAQ))
      (for
        ad      <- ActionDispatcher.make[IO]
        _       <- ad.dispatch(a1)
        _       <- ad.dispatch(a2)
        actions <- ad.pendingActions.take(2).compile.toList
      yield actions).asserting { _ mustBe List(a1, a2) }
    }
  }
}
