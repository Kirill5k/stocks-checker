package stockschecker.repositories.queries

import mongo4cats.operations.{Aggregate, Filter, Sort}
import stockschecker.repositories.InvestmentRankingRepository.Field

import java.time.Instant

private[repositories] object InvestmentRankingQueries:
  def notExpired(now: Instant): Filter =
    Filter.isNull(Field.ExpiresAt) || Filter.gt(Field.ExpiresAt, now)

  def latest(now: Instant): Aggregate =
    Aggregate.matchBy(notExpired(now)).sort(Sort.desc(Field.CreatedAt)).limit(1)
