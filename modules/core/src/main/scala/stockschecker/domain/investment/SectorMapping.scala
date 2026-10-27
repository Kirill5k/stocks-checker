package stockschecker.domain.investment

object SectorMapping:
  val Version = "finnhub-industry-v1"

  private val sectors = Map(
    "technology"                    -> "technology",
    "software"                      -> "technology",
    "semiconductors"                -> "technology",
    "electronic technology"         -> "technology",
    "it services"                   -> "technology",
    "hardware"                      -> "technology",
    "healthcare"                    -> "healthcare",
    "health care"                   -> "healthcare",
    "biotechnology"                 -> "healthcare",
    "pharmaceuticals"               -> "healthcare",
    "medical devices"               -> "healthcare",
    "industrials"                   -> "industrials",
    "industrial conglomerates"      -> "industrials",
    "aerospace & defense"           -> "industrials",
    "machinery"                     -> "industrials",
    "transportation"                -> "industrials",
    "professional services"         -> "industrials",
    "consumer cyclical"             -> "consumer-discretionary",
    "retail"                        -> "consumer-discretionary",
    "automobiles"                   -> "consumer-discretionary",
    "hotels, restaurants & leisure" -> "consumer-discretionary",
    "consumer defensive"            -> "consumer-staples",
    "consumer staples"              -> "consumer-staples",
    "food products"                 -> "consumer-staples",
    "beverages"                     -> "consumer-staples",
    "household products"            -> "consumer-staples",
    "energy"                        -> "energy",
    "oil & gas"                     -> "energy",
    "oil, gas & consumable fuels"   -> "energy",
    "utilities"                     -> "utilities",
    "basic materials"               -> "materials",
    "materials"                     -> "materials",
    "chemicals"                     -> "materials",
    "communication services"        -> "communication-services",
    "communications"                -> "communication-services",
    "media"                         -> "communication-services",
    "telecommunication"             -> "communication-services",
    "telecommunications"            -> "communication-services",
    "financial services"            -> "financials",
    "financials"                    -> "financials",
    "banking"                       -> "financials",
    "banks"                         -> "financials",
    "insurance"                     -> "financials",
    "real estate"                   -> "real-estate",
    "reit"                          -> "real-estate"
  )

  def sectorFor(industry: String): Option[String] = sectors.get(industry.trim.toLowerCase(java.util.Locale.ROOT))

