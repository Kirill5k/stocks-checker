package stockschecker.repositories

import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.operations.{Aggregate, Filter}

/** Visibility is a current read constraint; it never changes a screen's frozen membership. */
private[repositories] object InvestmentScreenVisibility:
  private val Holdings   = "_visibilityHoldings"
  private val Securities = "_visibilitySecurities"
  private val Profiles   = "_visibilityProfiles"
  private val Visible    = "_visibilityAllowed"

  val restrict: Aggregate =
    val holdings = Document(
      "$reduce" := Document(
        "input"        := "$cohorts.tickers",
        "initialValue" := List.empty[String],
        "in"           := Document("$setUnion" := List("$$value", "$$this"))
      )
    )
    val visible = Document(
      "$and" := List(
        Document("$setIsSubset" := List(s"$$$Holdings", s"$$$Securities.${SecurityRepository.Field.Id}")),
        Document("$setIsSubset" := List(s"$$$Holdings", s"$$$Profiles.${CompanyProfileRepository.Field.Id}"))
      )
    )
    Aggregate
      .set(Holdings -> holdings)
      .lookup(SecurityRepository.CollectionName, Holdings, SecurityRepository.Field.Id, Securities)
      .lookup(CompanyProfileRepository.CollectionName, Holdings, CompanyProfileRepository.Field.Id, Profiles)
      .matchBy(
        Filter.elemMatch(Securities, StockVisibility.activeSecurity.nor()).not &&
          Filter.elemMatch(Profiles, StockVisibility.activeProfile.nor()).not
      )
      .set(Visible -> visible)
      .matchBy(Filter.eq(Visible, true))
      .unset(Holdings, Securities, Profiles, Visible)
