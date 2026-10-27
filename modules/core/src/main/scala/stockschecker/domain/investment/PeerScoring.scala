package stockschecker.domain.investment

import stockschecker.domain.*

import scala.math.BigDecimal.RoundingMode

/** Ranks only complete candidates against the same fixed comparison universe. */
object PeerScoring:
  final case class Candidate(card: InvestmentScorecard, industry: Option[String], sector: Option[Sector])

  def rank(candidates: List[Candidate], recipe: InvestmentRecipe): List[InvestmentScorecard] =
    val complete = candidates.filter(_.card.exclusions.isEmpty)
    candidates
      .map { candidate =>
        if candidate.card.exclusions.nonEmpty then candidate.card
        else
          val industryPeers = complete.filter(_.industry == candidate.industry)
          val sectorPeers   = complete.filter(_.sector == candidate.sector)
          if industryPeers.size >= recipe.minIndustryPeers then
            score(candidate.card, s"industry:${candidate.industry.getOrElse("")}", industryPeers.map(_.card), recipe)
          else if sectorPeers.size >= recipe.minSectorPeers then
            score(candidate.card, s"sector:${candidate.sector.fold("")(_.print)}", sectorPeers.map(_.card), recipe)
          else
            candidate.card.copy(
              exclusions = List(InvestmentReasons.InsufficientPeerCoverage),
              peerCount = sectorPeers.size,
              weaknesses = candidate.card.weaknesses :+ "Too few companies with complete comparable data to assign a peer rank."
            )
      }
      .sortBy(card => (-card.score.getOrElse(BigDecimal(-1)), card.ticker.value))

  private def score(
      card: InvestmentScorecard,
      group: String,
      peers: List[InvestmentScorecard],
      recipe: InvestmentRecipe
  ): InvestmentScorecard =
    def percentile(feature: ScoringFeature, lowerIsBetter: Boolean = false): BigDecimal =
      val values  = peers.map(_.features(feature))
      val value   = card.features(feature)
      val less    = values.count(_ < value)
      val equal   = values.count(_ == value)
      val midrank = (BigDecimal(less) + BigDecimal(equal - 1) / 2) / (values.size - 1) * 100
      if lowerIsBetter then 100 - midrank else midrank
    val pillars = Map(
      Pillar.Quality -> rounded(
        (percentile(ScoringFeature.ReturnOnAssets5YMedian) + percentile(ScoringFeature.FreeCashFlowMargin5YMedian)) / 2
      ),
      Pillar.Value      -> rounded((percentile(ScoringFeature.EarningsYield) + percentile(ScoringFeature.FreeCashFlowYield)) / 2),
      Pillar.Growth     -> rounded((percentile(ScoringFeature.RevenueGrowth5Y) + percentile(ScoringFeature.EpsGrowth5Y)) / 2),
      Pillar.Resilience -> rounded(percentile(ScoringFeature.DebtToAssets, lowerIsBetter = true))
    )
    val contributions = recipe.weights.map { case (name, weight) => name -> (pillars(name) * weight) }
    val strengths     = pillars.toList.sortBy(_._1.print).collect {
      case (name, value) if value >= 75 => s"${name.print.capitalize} ranks in the top quarter of the $group cohort ($value/100)."
    }
    val weaknesses = pillars.toList.sortBy(_._1.print).collect {
      case (name, value) if value <= 25 => s"${name.print.capitalize} ranks in the bottom quarter of the $group cohort ($value/100)."
    }
    card.copy(
      eligible = true,
      score = Some(rounded(contributions.values.sum)),
      pillars = pillars,
      peerGroup = Some(group),
      peerCount = peers.size,
      strengths = strengths,
      weaknesses = weaknesses,
      contributions = contributions
    )

  private def rounded(value: BigDecimal): BigDecimal = value.setScale(2, RoundingMode.HALF_UP)
