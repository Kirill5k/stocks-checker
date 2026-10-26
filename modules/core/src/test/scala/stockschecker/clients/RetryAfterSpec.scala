package stockschecker.clients

import java.time.Instant
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers

class RetryAfterSpec extends AnyWordSpec with Matchers {
  "Retry-After parsing" should {
    "honor delay seconds and HTTP dates without truncating long provider cooldowns" in {
      val now = Instant.parse("2026-10-19T12:00:00Z")
      RetryAfter.deadline(Some("120"), now) mustBe now.plusSeconds(120)
      RetryAfter.deadline(Some("Tue, 20 Oct 2026 12:00:00 GMT"), now) mustBe now.plusSeconds(86400)
      RetryAfter.deadline(Some("172800"), now) mustBe now.plusSeconds(172800)
      RetryAfter.deadline(Some("bad-value"), now) mustBe now.plusSeconds(60)
      RetryAfter.deadline(None, now) mustBe now.plusSeconds(60)
    }
  }
}
