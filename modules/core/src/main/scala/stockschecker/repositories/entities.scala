package stockschecker.repositories

import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.codecs.MongoCodecProvider
import stockschecker.actions.Action
import stockschecker.domain.{Command, CommandId, CompanyProfile, CreateCommand, Exchange, Schedule, Security, SecurityKind, Ticker}

import java.time.{Instant, LocalDate}

private[repositories] object entities extends MongoJsonCodecs {

  final case class SecurityEntity(
      _id: Ticker,
      ticker: Ticker,
      exchange: Exchange,
      name: String,
      kind: SecurityKind,
      isActive: Boolean,
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: Security =
      Security(
        ticker = _id,
        exchange = exchange,
        name = name,
        kind = kind,
        isActive = isActive
      )

  object SecurityEntity:
    given MongoCodecProvider[SecurityEntity] = deriveCirceCodecProvider[SecurityEntity]
    def from(security: Security, now: Instant): SecurityEntity =
      SecurityEntity(
        _id = security.ticker,
        ticker = security.ticker,
        exchange = security.exchange,
        name = security.name,
        kind = security.kind,
        isActive = security.isActive,
        createdAt = now,
        updatedAt = now
      )

  final case class CompanyProfileEntity(
      _id: Ticker,
      name: String,
      country: String,
      industry: String,
      description: String,
      website: String,
      ipoDate: LocalDate,
      currency: String,
      marketCap: Long,
      createdAt: Instant,
      updatedAt: Instant
  ) derives Codec.AsObject:
    def toDomain: CompanyProfile =
      CompanyProfile(
        ticker = _id,
        name = name,
        country = country,
        industry = industry,
        description = description,
        website = website,
        ipoDate = ipoDate,
        currency = currency,
        marketCap = marketCap
      )

  object CompanyProfileEntity:
    given MongoCodecProvider[CompanyProfileEntity] = deriveCirceCodecProvider[CompanyProfileEntity]
    def from(profile: CompanyProfile, now: Instant): CompanyProfileEntity =
      CompanyProfileEntity(
        _id = profile.ticker,
        name = profile.name,
        country = profile.country,
        industry = profile.industry,
        description = profile.description,
        website = profile.website,
        ipoDate = profile.ipoDate,
        currency = profile.currency,
        marketCap = profile.marketCap,
        createdAt = now,
        updatedAt = now
      )

  final case class CommandEntity(
      _id: ObjectId,
      isActive: Boolean,
      action: Action,
      schedule: Schedule,
      lastExecutedAt: Option[Instant],
      executionCount: Int,
      maxExecutions: Option[Int]
  ) derives Codec.AsObject:
    def toDomain: Command =
      Command(
        id = CommandId(_id),
        isActive = isActive,
        action = action,
        schedule = schedule,
        lastExecutedAt = lastExecutedAt,
        executionCount = executionCount,
        maxExecutions = maxExecutions
      )

  object CommandEntity:
    given MongoCodecProvider[CommandEntity] = deriveCirceCodecProvider[CommandEntity]
    def from(cmd: Command): CommandEntity =
      CommandEntity(
        _id = cmd.id.toObjectId,
        isActive = cmd.isActive,
        action = cmd.action,
        schedule = cmd.schedule,
        lastExecutedAt = cmd.lastExecutedAt,
        executionCount = cmd.executionCount,
        maxExecutions = cmd.maxExecutions
      )
    def from(cmd: CreateCommand): CommandEntity =
      CommandEntity(
        _id = ObjectId.gen,
        isActive = true,
        action = cmd.action,
        schedule = cmd.schedule,
        lastExecutedAt = None,
        executionCount = 0,
        maxExecutions = cmd.maxExecutions
      )

}
