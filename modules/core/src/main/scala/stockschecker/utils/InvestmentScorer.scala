package stockschecker.utils

import stockschecker.domain.*

import java.time.{Instant, ZoneOffset}
import java.time.temporal.ChronoUnit
import scala.math.BigDecimal.RoundingMode

/** Scores a complete, fixed universe. Callers may filter the returned cards, never the peer inputs. */
object InvestmentScorer:
  val SectorMappingVersion = "finnhub-industry-v1"

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

  private case class Prepared(card: InvestmentScorecard, industry: Option[String], sector: Option[String])

  def rank(
      candidates: List[InvestmentCandidate],
      asOf: Instant,
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): List[InvestmentScorecard] =
    val weights = List(recipe.qualityWeight, recipe.valueWeight, recipe.growthWeight, recipe.resilienceWeight)
    require(weights.forall(_ >= 0) && weights.sum == 1, "Investment weights must be nonnegative and sum to one")
    require(recipe.minIndustryPeers >= 2 && recipe.minSectorPeers >= 2, "Peer cohorts require at least two companies")
    require(
      recipe.maxSnapshotAgeDays > 0 && recipe.maxAnnualAgeDays > 0 && recipe.maxPriceAgeDays > 0 && recipe.maxQuarterlyAgeDays > 0,
      "Freshness limits must be positive"
    )
    val duplicates = candidates.groupBy(_.stock.security.ticker).collect { case (ticker, rows) if rows.size > 1 => ticker }.toSet
    val prepared   = candidates.map { candidate =>
      val p = prepare(candidate, asOf, recipe)
      if duplicates.contains(candidate.stock.security.ticker) then
        p.copy(card =
          p.card.copy(
            exclusions = p.card.exclusions :+ "duplicate-ticker-in-universe",
            weaknesses = p.card.weaknesses :+ "Not ranked: ticker appears more than once in the input universe."
          )
        )
      else p
    }
    val complete = prepared.filter(_.card.exclusions.isEmpty)
    prepared
      .map { p =>
        if p.card.exclusions.nonEmpty then p.card
        else
          val industryPeers = complete.filter(_.industry == p.industry)
          val sectorPeers   = complete.filter(_.sector == p.sector)
          val cohort        =
            if industryPeers.size >= recipe.minIndustryPeers then Some((s"industry:${p.industry.getOrElse("")}", industryPeers))
            else if sectorPeers.size >= recipe.minSectorPeers then Some((s"sector:${p.sector.getOrElse("")}", sectorPeers))
            else None
          cohort match
            case None =>
              p.card.copy(
                exclusions = List("insufficient-peer-coverage"),
                peerCount = sectorPeers.size,
                weaknesses = p.card.weaknesses :+ "Too few companies with complete comparable data to assign a peer rank."
              )
            case Some((group, peers)) =>
              def percentile(feature: String, lowerIsBetter: Boolean = false): BigDecimal =
                val values  = peers.map(_.card.features(feature))
                val value   = p.card.features(feature)
                val less    = values.count(_ < value)
                val equal   = values.count(_ == value)
                val midrank = (BigDecimal(less) + BigDecimal(equal - 1) / 2) / (values.size - 1) * 100
                if lowerIsBetter then 100 - midrank else midrank
              val pillars = Map(
                "quality"    -> rounded((percentile("returnOnAssets5YMedian") + percentile("freeCashFlowMargin5YMedian")) / 2),
                "value"      -> rounded((percentile("earningsYield") + percentile("freeCashFlowYield")) / 2),
                "growth"     -> rounded((percentile("revenueGrowth5Y") + percentile("epsGrowth5Y")) / 2),
                "resilience" -> rounded(percentile("debtToAssets", lowerIsBetter = true))
              )
              val contributions = Map(
                "quality"    -> recipe.qualityWeight,
                "value"      -> recipe.valueWeight,
                "growth"     -> recipe.growthWeight,
                "resilience" -> recipe.resilienceWeight
              ).map { case (name, weight) =>
                name -> (pillars(name) * weight)
              }
              val score     = rounded(contributions.values.sum)
              val strengths = pillars.toList.sortBy(_._1).collect {
                case (name, value) if value >= 75 =>
                  s"${name.capitalize} ranks in the top quarter of the $group cohort ($value/100)."
              }
              val weaknesses = pillars.toList.sortBy(_._1).collect {
                case (name, value) if value <= 25 =>
                  s"${name.capitalize} ranks in the bottom quarter of the $group cohort ($value/100)."
              }
              p.card.copy(
                eligible = true,
                score = Some(score),
                pillars = pillars,
                peerGroup = Some(group),
                peerCount = peers.size,
                strengths = strengths,
                weaknesses = weaknesses,
                contributions = contributions
              )
      }
      .sortBy(card => (-card.score.getOrElse(BigDecimal(-1)), card.ticker.value))

  private def prepare(candidate: InvestmentCandidate, asOf: Instant, recipe: InvestmentRecipe): Prepared =
    val stock      = candidate.stock
    val industry   = stock.profile.map(_.industry.trim.toLowerCase(java.util.Locale.ROOT)).filter(_.nonEmpty)
    val sector     = industry.flatMap(sectorFor)
    val today      = asOf.atZone(ZoneOffset.UTC).toLocalDate
    val validPrice = stock.priceAnalytics.exists { analytics =>
      val p = analytics.performanceSummary
      p.latestPrice > 0 && !p.latestPriceDate.isAfter(today) && ChronoUnit.DAYS.between(p.latestPriceDate, today) <= recipe.maxPriceAgeDays
    }
    val stockReasons = List(
      Option.when(!stock.security.isActive)("inactive-security"),
      Option.when(stock.security.listingStatus != "listed")("security-not-listed"),
      stock.security.exclusionReason.map(reason => s"security-excluded:$reason"),
      Option.when(!validPrice)("missing-invalid-or-stale-price"),
      Option.when(stock.security.kind != SecurityKind.Stock)("specialist-security-model-required"),
      Option.when(!stock.profile.exists(_.country.equalsIgnoreCase("US")))("outside-us-company-universe"),
      Option.when(!stock.profile.exists(_.currency.equalsIgnoreCase("USD")))("unsupported-reporting-currency"),
      Option.when(sector.isEmpty)("unmapped-industry"),
      Option.when(sector.exists(s => s == "financials" || s == "real-estate"))("specialist-sector-model-required")
    ).flatten
    val (features, dataReasons, warnings) = candidate.financialSnapshot match
      case None           => (Map.empty[String, BigDecimal], List("missing-financial-snapshot"), Nil)
      case Some(snapshot) => featuresFrom(snapshot, stock.security.ticker, asOf, recipe)
    val priceWarnings = Option
      .when(!stock.priceAnalytics.exists(pa => pa.metrics.cagr5Year.nonEmpty && pa.metrics.volatility.nonEmpty))(
        "insufficient-comparable-price-risk-history"
      )
      .toList
    val evidence = candidate.financialSnapshot.map { snapshot =>
      val observations = snapshot.annual.groupBy(_.metric).toList.sortBy(_._1).flatMap { case (metric, points) =>
        if metric == "roa" then points.sortBy(_.period).reverse.take(5)
        else if metric == "fcfMargin" then points.sortBy(_.period).reverse.take(InvestmentEligibility.CashFlowHistoryYears)
        else if metric == "totalDebtToTotalAsset" then points.sortBy(_.period).reverse.take(1)
        else Nil
      }
      InvestmentEvidence(
        snapshot.provider,
        snapshot.observedAt,
        snapshot.mappingVersion,
        snapshot.payloadHash,
        observations.groupBy(_.metric).map { case (key, rows) => key -> rows.map(_.period) },
        observations,
        snapshot.quarterly.filter(_.metric == "fcfPerShareTTM").sortBy(_.period).lastOption,
        stock.priceAnalytics.map(_.toLatestPrice),
        stock.profile.map(_.currency)
      )
    }
    Prepared(
      InvestmentScorecard(
        stock.security.ticker,
        false,
        None,
        Map.empty,
        features,
        stockReasons ++ dataReasons,
        warnings ++ priceWarnings,
        None,
        0,
        candidate.financialSnapshot.map(_.id),
        recipe.version,
        features.keysIterator.map(key => key -> (if key == "epsTtm" then "reporting-currency-per-share" else "ratio")).toMap,
        evidence = evidence,
        weaknesses = (stockReasons ++ dataReasons).map(reason => s"Not ranked: ${reason.replace('-', ' ')}.")
      ),
      industry,
      sector
    )

  private def featuresFrom(
      snapshot: FinancialSnapshot,
      ticker: Ticker,
      asOf: Instant,
      recipe: InvestmentRecipe
  ): (Map[String, BigDecimal], List[String], List[String]) =
    val today   = asOf.atZone(ZoneOffset.UTC).toLocalDate
    val reasons = List.newBuilder[String]
    if snapshot.ticker != ticker then reasons += "financial-ticker-mismatch"
    if snapshot.provider != "Finnhub" || snapshot.mappingVersion != FinancialSnapshot.FinnhubMappingVersion then
      reasons += "unverified-financial-mapping"
    val requiredSeries = Set("annual:roa", "annual:fcfMargin", "annual:totalDebtToTotalAsset", "quarterly:fcfPerShareTTM")
    val relevantErrors = snapshot.validationErrors.filter { error =>
      error.split(":", 2).toList match
        case kind :: series :: Nil if Set("invalid-series", "invalid-observation", "duplicate-period")(kind) => requiredSeries(series)
        case _                                                                                               => true
    }
    if relevantErrors.nonEmpty then reasons += "invalid-provider-observations"
    if snapshot.observedAt.isAfter(asOf) then reasons += "financial-snapshot-from-future"
    if snapshot.observedAt.isBefore(asOf.minusSeconds(recipe.maxSnapshotAgeDays.toLong * 86400)) then reasons += "stale-financial-snapshot"
    def history(metric: String, count: Int): Option[List[FinancialObservation]] =
      val points   = snapshot.annual.filter(_.metric == metric).sortBy(_.period).reverse.take(count)
      val complete = points.size == count && points.forall(p => !p.period.isAfter(today)) &&
        points.sliding(2).forall {
          case List(newer, older) =>
            val days = ChronoUnit.DAYS.between(older.period, newer.period)
            days >= 300 && days <= 430
          case _ => true
        }
      if !complete then
        reasons += s"insufficient-annual-history:$metric"
        None
      else if points.exists(_.unit != "ratio") then
        reasons += s"unverified-unit:$metric"
        None
      else if ChronoUnit.DAYS.between(points.head.period, today) > recipe.maxAnnualAgeDays then
        reasons += s"stale-fiscal-period:$metric"
        None
      else Some(points)
    def positiveMetric(metric: String): Option[BigDecimal] =
      InvestmentEligibility.positiveMetric(metric, snapshot.metrics) match
        case Right(value) => Some(value)
        case Left(reason) =>
          reasons += reason
          None
    def growthMetric(metric: String): Option[BigDecimal] = snapshot.metrics.get(metric) match
      case Some(value) => Some(value / 100)
      case None        =>
        reasons += s"missing-metric:$metric"
        None
    def median(points: List[FinancialObservation]): BigDecimal =
      val values = points.map(_.value).sorted
      values(values.size / 2)
    val roa = history("roa", 5)
    val fcf = history("fcfMargin", InvestmentEligibility.CashFlowHistoryYears)
    if roa.nonEmpty && fcf.nonEmpty && roa.get.map(_.period) != fcf.get.map(_.period) then reasons += "misaligned-quality-fiscal-periods"
    val debt          = history("totalDebtToTotalAsset", 1)
    val pe            = positiveMetric("peTTM")
    val pfcf          = positiveMetric("pfcfShareTTM")
    val currentEps    = positiveMetric("epsTTM")
    val revenueGrowth = growthMetric("revenueGrowth5Y")
    val epsGrowth     = growthMetric("epsGrowth5Y")
    if fcf.exists(p => !InvestmentEligibility.hasDurableCashFlow(p)) then reasons += InvestmentEligibility.InsufficientCashFlowDurability
    if debt.exists(_.exists(_.value < 0)) then reasons += "negative-debt-to-assets"
    val features = List(
      roa.map(p => "returnOnAssets5YMedian" -> median(p)),
      fcf.map(p => "freeCashFlowMargin5YMedian" -> median(p)),
      pe.map(p => "earningsYield" -> (BigDecimal(1) / p)),
      pfcf.map(p => "freeCashFlowYield" -> (BigDecimal(1) / p)),
      revenueGrowth.map("revenueGrowth5Y" -> _),
      epsGrowth.map("epsGrowth5Y" -> _),
      debt.map(p => "debtToAssets" -> p.head.value),
      currentEps.map("epsTtm" -> _)
    ).flatten.toMap
    val latestFcf = snapshot.quarterly.filter(_.metric == "fcfPerShareTTM").sortBy(_.period).lastOption
    latestFcf match
      case Some(p) if p.unit != "reporting-currency-per-share" => reasons += "unverified-unit:fcfPerShareTTM"
      case Some(p) if p.value > 0 && !p.period.isAfter(today) && ChronoUnit.DAYS.between(p.period, today) <= recipe.maxQuarterlyAgeDays =>
        ()
      case Some(_) => reasons += "invalid-or-stale-current-free-cash-flow"
      case None    => reasons += "missing-current-free-cash-flow"
    val warnings = List(
      "weights-are-research-hypotheses",
      "growth-uses-provider-reported-five-year-metrics",
      s"sector-mapping:$SectorMappingVersion",
      "fiscal-history-is-not-point-in-time",
      "publication-time-unknown",
      "reporting-currency-unknown-no-cross-currency-yields-computed"
    )
    (features, reasons.result().distinct, warnings)

  private def rounded(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.HALF_UP)
