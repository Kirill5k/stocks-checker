package stockschecker.domain.investment

import stockschecker.domain.*

/** Applies display criteria to a published ranking without changing its scoring universe. */
final class InvestmentScreenSelection(
    entries: List[InvestmentRankingEntry],
    request: InvestmentScreenRequest
):
  private val inputs   = entries.filter(e => inScope(e.input, request)).map(e => e.scorecard.ticker -> e.input).toMap
  private val cards    = entries.map(_.scorecard)
  private val scoped   = cards.filter(c => inputs.contains(c.ticker))
  private val eligible = scoped.filter(_.eligible)
  private val assessed = eligible.map(c => c -> failedCriteria(c, request))
  private val matching = assessed.collect { case (card, Nil) => card }

  private def inScope(input: InvestmentScreenInput, request: InvestmentScreenRequest): Boolean =
    request.exchange.forall(_ == input.exchange) && request.country.forall(country => input.country.contains(country))

  def cohorts: List[InvestmentCohort] =
    def cohort(name: CohortName, tickers: List[Ticker]): InvestmentCohort =
      val count                                                                    = tickers.size
      def weights(group: InvestmentScreenInput => String): Map[String, BigDecimal] =
        if count == 0 then Map.empty
        else
          tickers
            .map(ticker => group(inputs(ticker)))
            .groupMapReduce(identity)(_ => 1)(_ + _)
            .view
            .mapValues(n => (BigDecimal(n) / count * 100).setScale(4, BigDecimal.RoundingMode.HALF_UP))
            .toMap
      val maxWeight  = if count == 0 then BigDecimal(0) else (BigDecimal(100) / count).setScale(4, BigDecimal.RoundingMode.HALF_UP)
      val herfindahl = if count == 0 then BigDecimal(0) else (BigDecimal(1) / count).setScale(6, BigDecimal.RoundingMode.HALF_UP)
      InvestmentCohort(
        name,
        tickers,
        Some(
          InvestmentConcentration(
            count,
            maxWeight,
            weights(_.sector.fold("unknown")(_.print)),
            weights(_.industry.getOrElse("unknown")),
            herfindahl
          )
        )
      )
    def top(name: CohortName, ranked: List[InvestmentScorecard]): InvestmentCohort =
      cohort(name, ranked.take(InvestmentRecipe.CohortSize).map(_.ticker))
    def byPillar(name: Pillar): List[InvestmentScorecard] =
      eligible.sortBy(c => (-c.pillars.getOrElse(name, BigDecimal(0)), c.ticker.value))
    val oldPrice = eligible
      .filter(c => inputs(c.ticker).priceScore.isDefined)
      .sortBy(c => (-inputs(c.ticker).priceScore.get, c.ticker.value))
    List(
      top(CohortName.Investment, matching),
      top(CohortName.OldPriceScore, oldPrice),
      top(CohortName.Quality, byPillar(Pillar.Quality)),
      top(CohortName.Value, byPillar(Pillar.Value)),
      cohort(CohortName.EqualUniverse, eligible.map(_.ticker).sortBy(_.value))
    )

  private def failedCriteria(card: InvestmentScorecard, request: InvestmentScreenRequest): List[String] =
    val pillarTests = List(
      Pillar.Quality    -> request.minQuality,
      Pillar.Value      -> request.minValue,
      Pillar.Growth     -> request.minGrowth,
      Pillar.Resilience -> request.minResilience
    ).map { case (pillar, minimum) => (pillar.print, minimum, card.pillars.get(pillar)) }
    val tests = ("score", request.minScore, card.score) :: pillarTests
    tests.flatMap { case (name, minimum, actual) =>
      minimum.filter(min => actual.forall(_ < min)).map(min => s"$name ${actual.fold("missing")(_.toString)} is below required $name $min")
    }

  def result(screen: InvestmentScreen): InvestmentScreenResult =
    val requestedNearMisses = assessed.collect { case (card, failed :: Nil) => InvestmentNearMiss(card, failed) }
    val financialNearMisses = scoped.filterNot(_.eligible).flatMap { card =>
      inputs(card.ticker).exclusionExplanation.map(InvestmentNearMiss(card, _))
    }
    val nearMisses = (requestedNearMisses ++ financialNearMisses).take(InvestmentRecipe.NearMissLimit)
    InvestmentScreenResult(
      screen.summary,
      screen.request,
      matching.take(screen.request.limit.getOrElse(InvestmentRecipe.CohortSize)),
      nearMisses,
      scoped.filterNot(_.eligible),
      screen.cohorts,
      screen.rankingId
    )
