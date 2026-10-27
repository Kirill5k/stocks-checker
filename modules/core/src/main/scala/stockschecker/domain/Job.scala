package stockschecker.domain

import io.circe.{Codec, Decoder, Encoder}
import stockschecker.actions.Action
import stockschecker.common.types.StringType
import sttp.tapir.{Codec as TapirCodec, DecodeResult, Schema}

import java.time.Instant
import java.util.UUID

opaque type JobId = String
object JobId extends StringType[JobId] {
  def gen: JobId = JobId(UUID.randomUUID().toString)

  inline given TapirCodec.PlainCodec[JobId] =
    TapirCodec.string.mapDecode[JobId](s => DecodeResult.Value(JobId(s)))(_.value)
  given Schema[JobId] = Schema.string
}

final case class JobCommand(commandId: CommandId, version: Long, runId: JobId) derives Codec.AsObject
final case class JobPlan(actions: List[Action], sequential: Boolean) derives Codec.AsObject

final case class Job(
    id: JobId,
    action: Action,
    availableAt: Instant,
    createdAt: Instant,
    command: Option[JobCommand] = None,
    dependsOn: List[JobId] = Nil,
    children: List[JobId] = Nil,
    state: JobState = Job.Pending,
    retries: Int = 0,
    leaseToken: Option[String] = None,
    leaseUntil: Option[Instant] = None,
    error: Option[String] = None,
    pausedProvider: Option[String] = None,
    plan: Option[JobPlan] = None,
    waitCount: Option[Int] = None,
    completedAt: Option[Instant] = None,
    restartCount: Option[Int] = None
) derives Codec.AsObject {
  def isTerminal: Boolean       = Job.terminalStates.contains(state)
  def hasUsableResults: Boolean = state == Job.Succeeded || state == Job.Partial
}

enum JobState(val value: String) {
  case Pending   extends JobState("pending")
  case Running   extends JobState("running")
  case Waiting   extends JobState("waiting")
  case Succeeded extends JobState("succeeded")
  case Partial   extends JobState("partial")
  case Failed    extends JobState("failed")
  case Cancelled extends JobState("cancelled")
  case Paused    extends JobState("paused")
}

object JobState {
  given Codec[JobState] = Codec.from(
    Decoder.decodeString.emap(value => values.find(_.value == value).toRight(s"Unknown job state: $value")),
    Encoder.encodeString.contramap(_.value)
  )
}

object Job {
  export JobState.*
  val terminalStates: List[JobState] = List(Succeeded, Partial, Failed, Cancelled)
}

final case class JobStatusSummary(counts: Map[JobState, Long]) {
  def count(state: JobState): Long = counts.getOrElse(state, 0L)
  def total: Long                  = counts.values.sum
  def terminal: Long               = Job.terminalStates.map(count).sum
  def usable: Long                 = count(Job.Succeeded) + count(Job.Partial)
}

/** Stored on the command before enqueueing, so a crash cannot lose an accepted scheduled occurrence. */
final case class ScheduledOccurrence(job: Job) derives Codec.AsObject

object ScheduledOccurrence {
  // The outbox is operational metadata, not an additional recursively derived command/action schema.
  given sttp.tapir.Schema[ScheduledOccurrence] = sttp.tapir.Schema.anyObject[ScheduledOccurrence]
}
