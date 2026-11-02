package stockschecker.repositories.queries

import mongo4cats.operations.{Aggregate, Filter}
import stockschecker.domain.ListingStatus
import stockschecker.repositories.{CompanyProfileRepository, SecurityRepository}

/** Current stock visibility. Administrative diagnostics deliberately do not use this restriction. */
private[repositories] object StockVisibility:
  private def field(path: String, name: String): String = if path.isEmpty then name else s"$path.$name"

  def activeSecurity: Filter = activeSecurity("")

  def activeSecurity(path: String): Filter =
    Filter.eq(field(path, SecurityRepository.Field.IsActive), true) &&
      Filter.eq(field(path, SecurityRepository.Field.ListingStatus), ListingStatus.Listed.print)

  def profileExists(path: String): Filter = Filter.exists(field(path, CompanyProfileRepository.Field.Id))

  def withActiveSecurity(tickerField: String): Aggregate =
    val security = "visibilitySecurity"
    Aggregate
      .lookup(SecurityRepository.CollectionName, tickerField, SecurityRepository.Field.Id, security)
      .matchBy(activeSecurity(s"$security.0"))
      .unset(security)

  def withProfile(tickerField: String): Aggregate =
    val profile = "visibilityProfile"
    Aggregate
      .lookup(CompanyProfileRepository.CollectionName, tickerField, CompanyProfileRepository.Field.Id, profile)
      .matchBy(profileExists(s"$profile.0"))
      .unset(profile)
