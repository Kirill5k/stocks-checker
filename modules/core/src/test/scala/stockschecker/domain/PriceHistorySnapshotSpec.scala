package stockschecker.domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{Instant, LocalDate}

class PriceHistorySnapshotSpec extends AnyWordSpec with Matchers {
  private val range    = DateRange(LocalDate.parse("2026-09-01"), LocalDate.parse("2026-09-30"))
  private val cutoff   = Instant.parse("2026-10-01T00:00:00Z")
  private val snapshot = PriceHistorySnapshot(
    "completed-history",
    Ticker("TEST"),
    Nil,
    Provider.TwelveData,
    "USD",
    PriceAdjustment.SplitsAndDividends,
    cutoff,
    range
  )

  "PriceHistorySnapshot" should {
    "use the next UTC midnight across leap days, year ends and daylight-saving transitions" in
      List(
        "2024-02-29" -> "2024-03-01T00:00:00Z",
        "2026-12-31" -> "2027-01-01T00:00:00Z",
        "2026-03-29" -> "2026-03-30T00:00:00Z",
        "2026-10-25" -> "2026-10-26T00:00:00Z"
      ).foreach { case (through, expected) =>
        PriceHistorySnapshot.completedSessionCutoff(LocalDate.parse(through)) mustBe Instant.parse(expected)
      }

    "accept a completed final session exactly at its cutoff and reject an earlier observation" in {
      snapshot.copy(fetchedAt = cutoff.minusNanos(1)).coversCompletedSessions(range) mustBe false
      snapshot.coversCompletedSessions(range) mustBe true
      snapshot.copy(fetchedAt = cutoff.plusNanos(1)).coversCompletedSessions(range) mustBe true
    }

    "require coverage of both inclusive range boundaries even after the final session is complete" in {
      val broader = range.copy(from = range.from.minusDays(1), through = range.through.plusDays(1))
      snapshot.copy(requestedRange = broader).coversCompletedSessions(range) mustBe true
      snapshot.copy(requestedRange = range.copy(from = range.from.plusDays(1))).coversCompletedSessions(range) mustBe false
      snapshot.copy(requestedRange = range.copy(through = range.through.minusDays(1))).coversCompletedSessions(range) mustBe false
    }
  }
}
