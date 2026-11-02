package stockschecker.domain.investment

import stockschecker.domain.*

import java.time.Instant

/** Scores a complete, fixed universe using a recipe validated when services are constructed. */
object InvestmentScorer:
  def rank(
      candidates: List[InvestmentCandidate],
      asOf: Instant,
      recipe: InvestmentRecipe = InvestmentRecipe()
  ): List[InvestmentScorecard] =
    PeerScoring.rank(candidates.map(prepare(_, asOf, recipe)), recipe)

  private def prepare(candidate: InvestmentCandidate, asOf: Instant, recipe: InvestmentRecipe): PeerScoring.Candidate =
    val stock     = candidate.stock
    val industry  = Some(InvestmentScreenInput.industryKey(stock.profile.industry)).filter(_.nonEmpty)
    val sector    = stock.profile.sector
    val extracted = candidate.financialSnapshot match
      case None           => Left(FeatureExtraction.Failure(Map.empty, List(InvestmentReasons.MissingFinancialSnapshot)))
      case Some(snapshot) => FeatureExtraction.from(snapshot, stock.security.ticker, asOf, recipe)
    val (features, dataReasons) = extracted.fold(failure => (failure.features, failure.reasons), features => (features, Nil))
    val reasons                 = Eligibility.stockReasons(stock, asOf, recipe) ++ dataReasons
    val warnings = candidate.financialSnapshot.toList.flatMap(_ => FeatureExtraction.Warnings) ++ Eligibility.priceWarnings(stock)
    PeerScoring.Candidate(
      InvestmentScorecard(
        stock.security.ticker,
        false,
        None,
        Map.empty,
        features,
        reasons,
        warnings,
        None,
        0,
        recipe.version,
        features.keysIterator.map(key => key -> (if key == ScoringFeature.EpsTtm then "reporting-currency-per-share" else "ratio")).toMap,
        evidence = candidate.financialSnapshot.map(evidence(_, stock)),
        weaknesses = reasons.map(reason => s"Not ranked: ${reason.replace('-', ' ')}.")
      ),
      industry,
      sector
    )

  private def evidence(snapshot: FinancialSnapshot, stock: Stock): InvestmentEvidence =
    val observations = snapshot.annual.groupBy(_.metric).toList.sortBy(_._1.print).flatMap { case (metric, points) =>
      if metric == FinancialMetric.ReturnOnAssets then points.sortBy(_.period).reverse.take(5)
      else if metric == FinancialMetric.FreeCashFlowMargin then points.sortBy(_.period).reverse.take(Eligibility.CashFlowHistoryYears)
      else if metric == FinancialMetric.DebtToAssets then points.sortBy(_.period).reverse.take(1)
      else Nil
    }
    InvestmentEvidence(
      snapshot.provider,
      snapshot.observedAt,
      snapshot.mappingVersion,
      snapshot.payloadHash,
      observations.groupBy(_.metric).map { case (key, rows) => key -> rows.map(_.period) },
      observations,
      snapshot.quarterly.filter(_.metric == FinancialMetric.FreeCashFlowPerShareTtm).sortBy(_.period).lastOption,
      stock.priceAnalytics.map(_.toLatestPrice),
      Some(stock.profile.currency)
    )
