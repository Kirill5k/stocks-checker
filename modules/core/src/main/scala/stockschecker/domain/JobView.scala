package stockschecker.domain

import io.circe.Codec
import stockschecker.actions.Action

import java.time.Instant
import java.util.regex.Matcher

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
  // Stored messages retain useful provider diagnostics. Scrub common credential fields from old messages too.
  private val Credentials =
    """(?i)(["']?(?:api[-_]?key|token|access[-_]?token|password|authorization)["']?\s*[:=]\s*)(?:"[^"]*"|'[^']*'|[^\s&,;}]+)""".r
  private val Authentication = """(?i)\b(Bearer|Basic)\s+[^\s,"'}]+""".r

  private def redact(message: String): String =
    val authentication = Authentication.replaceAllIn(message, matched => s"${matched.group(1)} [redacted]")
    Credentials.replaceAllIn(authentication, matched => Matcher.quoteReplacement(s"${matched.group(1)}[redacted]"))

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
      job.command.map(_.commandId),
      job.command.map(_.runId),
      job.dependsOn,
      job.children,
      job.pausedProvider,
      job.error.map(redact)
    )
