package stockschecker.domain

import java.time.LocalDate

/** Inclusive calendar-date bounds; clients validate the ordering before issuing a request. */
final case class DateRange(from: LocalDate, through: LocalDate) {
  def isValid: Boolean = !from.isAfter(through)

  def contains(date: LocalDate): Boolean = !date.isBefore(from) && !date.isAfter(through)
}
