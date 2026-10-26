package stockschecker.actions

import cats.effect.IO
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import org.mockito.ArgumentCaptor
import stockschecker.domain.Job
import stockschecker.fixtures.*
import stockschecker.repositories.JobRepository

class ActionDispatcherSpec extends IOWordSpec {
  given Clock[IO] = Clock.mock(ts)

  "An ActionDispatcher" should {
    "persist accepted work before returning" in {
      val jobs   = mock[JobRepository[IO]]
      val action = Action.DeactivateSecurity(AAPL)
      when(jobs.enqueue(any[Job])).thenReturnUnit
      val captured = ArgumentCaptor.forClass(classOf[Job])
      (for {
        dispatcher <- ActionDispatcher.durable(jobs)
        _          <- dispatcher.dispatch(action)
      } yield ()).asserting { _ =>
        verify(jobs).enqueue(captured.capture())
        captured.getValue.action mustBe action
        captured.getValue.availableAt mustBe ts
        captured.getValue.createdAt mustBe ts
        captured.getValue.id.value must not be empty
      }
    }
  }
}
