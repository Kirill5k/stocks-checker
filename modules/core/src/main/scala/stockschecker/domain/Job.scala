package stockschecker.domain

import io.circe.Codec
import stockschecker.actions.Action

import java.time.Instant

final case class JobCommand(commandId: CommandId, version: Long, runId: String) derives Codec.AsObject
final case class JobPlan(actions: List[Action], sequential: Boolean) derives Codec.AsObject

final case class Job(
    id: String,
    action: Action,
    availableAt: Instant,
    createdAt: Instant,
    command: Option[JobCommand] = None,
    dependsOn: List[String] = Nil,
    children: List[String] = Nil,
    state: String = Job.Pending,
    retries: Int = 0,
    leaseToken: Option[String] = None,
    leaseUntil: Option[Instant] = None,
    error: Option[String] = None,
    pausedProvider: Option[String] = None,
    plan: Option[JobPlan] = None
) derives Codec.AsObject {
  def isTerminal: Boolean = Job.terminalStates.contains(state)
  def hasUsableResults: Boolean = state == Job.Succeeded || state == Job.Partial
}

object Job {
  val Pending   = "pending"
  val Running   = "running"
  val Waiting   = "waiting"
  val Succeeded = "succeeded"
  val Partial   = "partial"
  val Failed    = "failed"
  val Cancelled = "cancelled"
  val Paused    = "paused"
  val terminalStates: List[String] = List(Succeeded, Partial, Failed, Cancelled)
}

/** Stored on the command before enqueueing, so a crash cannot lose an accepted scheduled occurrence. */
final case class ScheduledOccurrence(job: Job) derives Codec.AsObject

object ScheduledOccurrence {
  // The outbox is operational metadata, not an additional recursively derived command/action schema.
  given sttp.tapir.Schema[ScheduledOccurrence] = sttp.tapir.Schema.anyObject[ScheduledOccurrence]
}
