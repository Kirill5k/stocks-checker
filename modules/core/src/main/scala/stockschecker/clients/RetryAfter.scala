package stockschecker.clients

import java.time.{Instant, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.util.Try

object RetryAfter:
  /** RFC 9110 permits both delay-seconds and an HTTP-date. Invalid headers use a conservative minute. */
  def deadline(value: Option[String], now: Instant): Instant =
    value
      .flatMap { raw =>
        val text = raw.trim
        text.toLongOption
          .filter(_ >= 0)
          .flatMap(seconds => Try(now.plusSeconds(seconds.max(1).min(86400))).toOption)
          .orElse(Try(ZonedDateTime.parse(text, DateTimeFormatter.RFC_1123_DATE_TIME).toInstant).toOption)
      }
      .map(deadline => if deadline.isAfter(now.plusSeconds(86400)) then now.plusSeconds(86400) else deadline)
      .filter(_.isAfter(now))
      .getOrElse(now.plusSeconds(60))
