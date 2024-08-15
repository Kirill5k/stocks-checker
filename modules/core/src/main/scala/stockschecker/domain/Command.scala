package stockschecker.domain

import io.circe.Codec
import kirill5k.common.syntax.time.*
import stockschecker.actions.Action
import stockschecker.common.types.IdType

import java.time.Instant
import scala.concurrent.duration.{Duration, FiniteDuration}

opaque type CommandId = String
object CommandId extends IdType[CommandId]

final case class CreateCommand(
    action: Action,
    schedule: Schedule,
    maxExecutions: Option[Int]
) derives Codec.AsObject

final case class Command(
    id: CommandId,
    isActive: Boolean,
    action: Action,
    schedule: Schedule,
    lastExecutedAt: Option[Instant],
    executionCount: Int,
    maxExecutions: Option[Int]
) derives Codec.AsObject {
  def durationUntilNextExecution(now: Instant): FiniteDuration =
    lastExecutedAt
      .map(schedule.nextExecutionTime)
      .filter(_.isAfter(now))
      .map(now.durationBetween)
      .getOrElse {
        schedule match
          case _: Schedule.Periodic => Duration.Zero
          case _: Schedule.Cron     => now.durationBetween(schedule.nextExecutionTime(now))
      }
}
