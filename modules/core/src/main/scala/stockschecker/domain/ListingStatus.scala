package stockschecker.domain

import stockschecker.common.types.EnumType

enum ListingStatus:
  case Listed, Delisted

object ListingStatus extends EnumType[ListingStatus](() => ListingStatus.values)
