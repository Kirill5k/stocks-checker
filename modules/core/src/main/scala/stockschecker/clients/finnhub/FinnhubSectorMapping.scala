package stockschecker.clients.finnhub

import stockschecker.domain.Sector

object FinnhubSectorMapping:
  val Version = "finnhub-industry-v1"

  private val sectors = Map(
    "technology"                    -> Sector.Technology,
    "software"                      -> Sector.Technology,
    "semiconductors"                -> Sector.Technology,
    "electronic technology"         -> Sector.Technology,
    "it services"                   -> Sector.Technology,
    "hardware"                      -> Sector.Technology,
    "healthcare"                    -> Sector.Healthcare,
    "health care"                   -> Sector.Healthcare,
    "biotechnology"                 -> Sector.Healthcare,
    "pharmaceuticals"               -> Sector.Healthcare,
    "medical devices"               -> Sector.Healthcare,
    "industrials"                   -> Sector.Industrials,
    "industrial conglomerates"      -> Sector.Industrials,
    "aerospace & defense"           -> Sector.Industrials,
    "machinery"                     -> Sector.Industrials,
    "transportation"                -> Sector.Industrials,
    "professional services"         -> Sector.Industrials,
    "consumer cyclical"             -> Sector.ConsumerDiscretionary,
    "retail"                        -> Sector.ConsumerDiscretionary,
    "automobiles"                   -> Sector.ConsumerDiscretionary,
    "hotels, restaurants & leisure" -> Sector.ConsumerDiscretionary,
    "consumer defensive"            -> Sector.ConsumerStaples,
    "consumer staples"              -> Sector.ConsumerStaples,
    "food products"                 -> Sector.ConsumerStaples,
    "beverages"                     -> Sector.ConsumerStaples,
    "household products"            -> Sector.ConsumerStaples,
    "energy"                        -> Sector.Energy,
    "oil & gas"                     -> Sector.Energy,
    "oil, gas & consumable fuels"   -> Sector.Energy,
    "utilities"                     -> Sector.Utilities,
    "basic materials"               -> Sector.Materials,
    "materials"                     -> Sector.Materials,
    "chemicals"                     -> Sector.Materials,
    "communication services"        -> Sector.CommunicationServices,
    "communications"                -> Sector.CommunicationServices,
    "media"                         -> Sector.CommunicationServices,
    "telecommunication"             -> Sector.CommunicationServices,
    "telecommunications"            -> Sector.CommunicationServices,
    "financial services"            -> Sector.Financials,
    "financials"                    -> Sector.Financials,
    "banking"                       -> Sector.Financials,
    "banks"                         -> Sector.Financials,
    "insurance"                     -> Sector.Financials,
    "real estate"                   -> Sector.RealEstate,
    "reit"                          -> Sector.RealEstate
  )

  def sectorFor(industry: String): Option[Sector] = sectors.get(industry.trim.toLowerCase(java.util.Locale.ROOT))
