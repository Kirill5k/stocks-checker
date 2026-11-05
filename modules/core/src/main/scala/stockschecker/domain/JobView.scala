package stockschecker.domain

import io.circe.Codec
import stockschecker.actions.Action

import java.time.Instant

final case class JobFilter(
    commandId: Option[CommandId] = None,
    state: Option[JobState] = None,
    actionKind: Option[String] = None
)

final case class JobView(
    id: JobId,
    action: Action,
    state: JobState,
    createdAt: Instant,
    availableAt: Instant,
    completedAt: Option[Instant],
    retries: Int,
    waitCount: Int,
    commandId: Option[CommandId],
    runId: Option[JobId],
    dependsOn: List[JobId],
    children: List[JobId],
    pausedProvider: Option[Provider],
    error: Option[String]
) derives Codec.AsObject

object JobView:
  def from(job: Job): JobView =
    JobView(
      job.id,
      job.action,
      job.state,
      job.createdAt,
      job.availableAt,
      job.completedAt,
      job.retries,
      job.waitCount,
      job.commandId,
      job.command.map(_.runId),
      job.dependsOn,
      job.children,
      job.pausedProvider,
      job.error
    )
