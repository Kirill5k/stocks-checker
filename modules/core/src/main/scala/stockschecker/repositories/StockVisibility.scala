package stockschecker.repositories

import mongo4cats.operations.{Aggregate, Filter}
import stockschecker.domain.ListingStatus

/** Current stock visibility. Administrative diagnostics deliberately do not use this restriction. */
private[repositories] object StockVisibility:
  private def field(path: String, name: String): String = if path.isEmpty then name else s"$path.$name"

  def activeSecurity: Filter = activeSecurity("")

  def activeSecurity(path: String): Filter =
    Filter.eq(field(path, SecurityRepository.Field.IsActive), true) &&
      Filter.eq(field(path, SecurityRepository.Field.ListingStatus), ListingStatus.Listed.print)

  def activeProfile: Filter = activeProfile("")

  def activeProfile(path: String): Filter = Filter.eq(field(path, CompanyProfileRepository.Field.IsActive), true)

  def withActiveSecurity(tickerField: String): Aggregate =
    val security = "visibilitySecurity"
    Aggregate
      .lookup(SecurityRepository.CollectionName, tickerField, SecurityRepository.Field.Id, security)
      .matchBy(activeSecurity(s"$security.0"))
      .unset(security)

  def withActiveProfile(tickerField: String): Aggregate =
    val profile = "visibilityProfile"
    Aggregate
      .lookup(CompanyProfileRepository.CollectionName, tickerField, CompanyProfileRepository.Field.Id, profile)
      .matchBy(activeProfile(s"$profile.0"))
      .unset(profile)

  def restrict(tickerField: String): Aggregate =
    withActiveSecurity(tickerField).combinedWith(withActiveProfile(tickerField))
