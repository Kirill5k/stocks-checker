package stockschecker.domain

import stockschecker.common.types.EnumType

enum Sector:
  case Technology, Healthcare, Industrials, ConsumerDiscretionary, ConsumerStaples, Energy, Utilities, Materials
  case CommunicationServices, Financials, RealEstate

object Sector extends EnumType[Sector](() => Sector.values):
  val requiresSpecialistModel: Set[Sector] = Set(Financials, RealEstate)
