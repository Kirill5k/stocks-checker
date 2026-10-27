package stockschecker.utils

import stockschecker.domain.*

/** Applies display criteria to a published ranking without changing its scoring universe. */
final class InvestmentScreenSelection(
    inputs: List[InvestmentCandidate],
    cards: List[InvestmentScorecard],
    request: InvestmentScreenRequest
):
  private val stocks   = inputs.filter(inScope(_, request)).map(i => i.stock.security.ticker -> i.stock).toMap
  private val scoped   = cards.filter(c => stocks.contains(c.ticker))
  private val eligible = scoped.filter(_.eligible)
  private val assessed = eligible.map(c => c -> failedCriteria(c, request))
  private val matching = assessed.collect { case (card, Nil) => card }

  private def inScope(input: InvestmentCandidate, request: InvestmentScreenRequest): Boolean =
    request.exchange.forall(_ == input.stock.security.exchange) &&
      request.country.forall(country => input.stock.profile.exists(_.country == country))

  def cohorts: List[InvestmentCohort] =
    def cohort(name: String, tickers: List[Ticker]): InvestmentCohort =
      val count                                                    = tickers.size
      def weights(group: Stock => String): Map[String, BigDecimal] =
        if count == 0 then Map.empty
        else
          tickers
            .map(ticker => group(stocks(ticker)))
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
            weights(s => s.profile.flatMap(p => InvestmentScorer.sectorFor(p.industry)).getOrElse("unknown")),
            weights(s => s.profile.map(_.industry).getOrElse("unknown")),
            herfindahl
          )
        )
      )
    def top(name: String, ranked: List[InvestmentScorecard]): InvestmentCohort = cohort(name, ranked.take(20).map(_.ticker))
    def byPillar(name: String): List[InvestmentScorecard]                      =
      eligible.sortBy(c => (-c.pillars.getOrElse(name, BigDecimal(0)), c.ticker.value))
    val oldPrice = eligible
      .filter(c => stocks(c.ticker).priceAnalytics.isDefined)
      .sortBy(c => (-stocks(c.ticker).priceAnalytics.get.scores.overallScore, c.ticker.value))
    List(
      top("investment", matching),
      top("old-price-score", oldPrice),
      top("quality", byPillar("quality")),
      top("value", byPillar("value")),
      cohort("equal-universe", eligible.map(_.ticker).sortBy(_.value))
    )

  private def failedCriteria(card: InvestmentScorecard, request: InvestmentScreenRequest): List[String] =
    val tests = List(
      ("score", request.minScore, card.score),
      ("quality", request.minQuality, card.pillars.get("quality")),
      ("value", request.minValue, card.pillars.get("value")),
      ("growth", request.minGrowth, card.pillars.get("growth")),
      ("resilience", request.minResilience, card.pillars.get("resilience"))
    )
    tests.flatMap { case (name, minimum, actual) =>
      minimum.filter(min => actual.forall(_ < min)).map(min => s"$name ${actual.fold("missing")(_.toString)} is below required $name $min")
    }

  def result(run: InvestmentScreenRun): InvestmentScreenResult =
    val requestedNearMisses = assessed.collect { case (card, failed :: Nil) => InvestmentNearMiss(card, failed) }
    val snapshotByTicker    = inputs.flatMap(i => i.financialSnapshot.map(i.stock.security.ticker -> _)).toMap
    val financialNearMisses = scoped.filterNot(_.eligible).flatMap { card =>
      val explanation = InvestmentEligibility.explainExclusion(card, snapshotByTicker.get(card.ticker))
      explanation.map(InvestmentNearMiss(card, _))
    }
    val nearMisses = (requestedNearMisses ++ financialNearMisses).take(10)
    InvestmentScreenResult(
      InvestmentScreenSummary.from(run),
      run.request,
      matching.take(run.request.limit.getOrElse(20)),
      nearMisses,
      scoped.filterNot(_.eligible),
      run.cohorts,
      run.rankingId
    )
