package stockschecker.domain.investment

import stockschecker.domain.*

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate}

class InvestmentScorerSpec extends AnyWordSpec with Matchers:
  private val now    = Instant.parse("2026-01-15T12:00:00Z")
  private val recipe = InvestmentRecipe(minIndustryPeers = 2, minSectorPeers = 3)

  private def candidate(
      symbol: String,
      strength: Int = 1,
      industry: String = "Technology",
      sector: Sector = Sector.Technology
  ): InvestmentCandidate =
    val ticker       = Ticker(symbol)
    val observations = (0 until 6).toList.flatMap { i =>
      val period = LocalDate.of(2025 - i, 9, 30)
      List(
        FinancialObservation(FinancialMetric.ReturnOnAssets, period, BigDecimal("0.1") * strength),
        FinancialObservation(FinancialMetric.FreeCashFlowMargin, period, BigDecimal("0.1") * strength),
        FinancialObservation(FinancialMetric.DebtToAssets, period, BigDecimal("0.6") / strength)
      )
    }
    val snapshot = FinancialSnapshot(
      symbol,
      ticker,
      Provider.Finnhub,
      now,
      Map(
        FinancialMetric.PriceToEarningsTtm     -> (BigDecimal(20) / strength),
        FinancialMetric.PriceToFreeCashFlowTtm -> (BigDecimal(30) / strength),
        FinancialMetric.EpsTtm                 -> BigDecimal(10),
        FinancialMetric.RevenueGrowth5Y        -> (BigDecimal("0.1") * strength),
        FinancialMetric.EpsGrowth5Y            -> (BigDecimal("0.15") * strength)
      ),
      observations,
      List(FinancialObservation(FinancialMetric.FreeCashFlowPerShareTtm, LocalDate.of(2025, 9, 30), BigDecimal(5))),
      "hash",
      "canonical-test-v1",
      Nil
    )
    val analytics = AAPLPriceAnalytics.copy(
      ticker = ticker,
      performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(latestPriceDate = LocalDate.of(2026, 1, 15))
    )
    InvestmentCandidate(
      Stock(
        AAPLSecurity.copy(ticker = ticker),
        Some(AAPLCompanyProfile.copy(ticker = ticker, industry = industry, sector = Some(sector))),
        Some(analytics)
      ),
      Some(snapshot)
    )

  "InvestmentScorer" should {
    "rank stronger financial fundamentals with separately explained pillars" in {
      val cards = InvestmentScorer.rank(List(candidate("LOW"), candidate("HIGH", 2)), now, recipe)
      cards.map(_.ticker.value) mustBe List("HIGH", "LOW")
      cards.head.score mustBe Some(BigDecimal(100))
      cards.last.score mustBe Some(BigDecimal(0))
      cards.head.pillars.keySet mustBe Set(Pillar.Quality, Pillar.Value, Pillar.Growth, Pillar.Resilience)
      cards.head.features(ScoringFeature.RevenueGrowth5Y) mustBe BigDecimal("0.2")
      cards.head.warnings must contain("publication-time-unknown")
      cards.head.contributions mustBe Map(
        Pillar.Quality    -> BigDecimal(35),
        Pillar.Value      -> BigDecimal(30),
        Pillar.Growth     -> BigDecimal(20),
        Pillar.Resilience -> BigDecimal(15)
      )
      cards.head.strengths must have size 4
      cards.last.weaknesses must have size 4
      val evidence = cards.head.evidence.get
      evidence.provider mustBe Provider.Finnhub
      evidence.observedAt mustBe now
      evidence.annualPeriods(FinancialMetric.ReturnOnAssets) mustBe (2025 to 2021 by -1).toList.map(year => LocalDate.of(year, 9, 30))
      evidence.financialObservations.count(_.metric == FinancialMetric.DebtToAssets) mustBe 1
      evidence.latestFreeCashFlowPerShareTtm.map(_.value) mustBe Some(BigDecimal(5))
      cards.head.featureUnits(ScoringFeature.EpsTtm) mustBe "reporting-currency-per-share"
    }

    "give tied observations midrank and order tied tickers deterministically" in {
      val inputs = List(candidate("ZZZ"), candidate("AAA"))
      val cards  = InvestmentScorer.rank(inputs, now, recipe)
      cards.map(_.score) mustBe List(Some(BigDecimal(50)), Some(BigDecimal(50)))
      cards.map(_.ticker.value) mustBe List("AAA", "ZZZ")
      InvestmentScorer.rank(inputs.reverse, now, recipe) mustBe cards
    }

    "require twenty complete industry peers by default" in {
      val inputs = (1 to 20).toList.map(i => candidate(s"S$i"))
      InvestmentScorer.rank(inputs, now).forall(_.eligible) mustBe true
      InvestmentScorer.rank(inputs.tail, now).forall(_.exclusions.contains("insufficient-peer-coverage")) mustBe true
    }

    "fall back to a sufficiently populated mapped sector" in {
      val inputs =
        List(candidate("A", industry = "Software"), candidate("B", industry = "Hardware"), candidate("C", industry = "Semiconductors"))
      val cards = InvestmentScorer.rank(inputs, now, recipe)
      cards.forall(_.peerGroup.contains("sector:technology")) mustBe true
      cards.forall(_.peerCount == 3) mustBe true
    }

    "exclude missing value inputs instead of redistributing their weights" in {
      val incomplete = candidate("MISSING", 2)
      val modified   =
        incomplete.copy(financialSnapshot =
          incomplete.financialSnapshot.map(s => s.copy(metrics = s.metrics - FinancialMetric.PriceToFreeCashFlowTtm))
        )
      val cards    = InvestmentScorer.rank(List(candidate("A"), candidate("B"), modified), now, recipe)
      val excluded = cards.find(_.ticker == Ticker("MISSING")).get
      excluded.score mustBe None
      excluded.exclusions must contain("missing-metric:price-to-free-cash-flow-ttm")
      cards.filter(_.eligible).forall(_.peerCount == 2) mustBe true
    }

    "exclude unavailable reported revenue growth rather than substituting sales per share" in {
      val input    = candidate("A")
      val modified =
        input.copy(financialSnapshot = input.financialSnapshot.map(s => s.copy(metrics = s.metrics - FinancialMetric.RevenueGrowth5Y)))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain("missing-metric:revenue-growth5y")
      card.score mustBe None
    }

    "reject typed validation issues on canonical observations" in {
      val input                                        = candidate("A")
      def ranked(issues: List[FinancialSnapshotIssue]) = InvestmentScorer
        .rank(
          List(
            input.copy(financialSnapshot = input.financialSnapshot.map(_.copy(issues = issues))),
            candidate("B")
          ),
          now,
          recipe
        )
        .find(_.ticker == Ticker("A"))
        .get
      ranked(Nil).eligible mustBe true
      ranked(List(FinancialSnapshotIssue.InvalidObservation("annual", FinancialMetric.ReturnOnAssets))).exclusions must contain(
        "invalid-provider-observations"
      )
      ranked(List(FinancialSnapshotIssue.SymbolMismatch)).exclusions must contain("invalid-provider-observations")
    }

    "reject stale fetches and incomplete fiscal histories without interpreting mapping versions" in {
      val input    = candidate("A")
      val modified = input.copy(financialSnapshot =
        input.financialSnapshot.map(s =>
          s.copy(
            observedAt = now.minusSeconds(15L * 86400),
            mappingVersion = "unknown",
            annual = s.annual.filterNot(p => p.metric == FinancialMetric.ReturnOnAssets && p.period.getYear == 2023)
          )
        )
      )
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain allOf ("stale-financial-snapshot", "insufficient-annual-history:return-on-assets")
      card.score mustBe None
    }

    "exclude current losses without inventing a CAGR from negative optional history" in {
      val input    = candidate("A")
      val modified = input.copy(financialSnapshot =
        input.financialSnapshot.map(s =>
          s.copy(
            metrics = s.metrics.updated(FinancialMetric.EpsTtm, BigDecimal(-1))
          )
        )
      )
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain("nonpositive-metric:eps-ttm")
    }

    "require quality histories to cover the same fiscal periods" in {
      val input    = candidate("A")
      val modified = input.copy(financialSnapshot =
        input.financialSnapshot.map(s =>
          s.copy(
            annual =
              s.annual.map(p => if p.metric == FinancialMetric.FreeCashFlowMargin then p.copy(period = p.period.minusYears(1)) else p)
          )
        )
      )
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain("misaligned-quality-fiscal-periods")
      card.exclusions must not contain "insufficient-annual-history:free-cash-flow-margin"
      card.score mustBe None
    }

    "derive observation units from their canonical metrics" in {
      val snapshot = candidate("A").financialSnapshot.get
      snapshot.annual.map(_.unit).distinct mustBe List(MetricUnit.Ratio)
      snapshot.quarterly.map(_.unit).distinct mustBe List(MetricUnit.PerShare)
    }

    "accept canonical snapshots with opaque provider provenance and mapping versions" in {
      val input    = candidate("A")
      val modified = input.copy(financialSnapshot =
        input.financialSnapshot.map(_.copy(provider = Provider.TwelveData, mappingVersion = "another-mapping"))
      )
      InvestmentScorer.rank(List(modified, candidate("B")), now, recipe).forall(_.eligible) mustBe true
    }

    "warn separately about missing price risk history without changing financial eligibility" in {
      val input    = candidate("A")
      val modified = input.copy(stock =
        input.stock.copy(priceAnalytics =
          input.stock.priceAnalytics.map(p => p.copy(metrics = p.metrics.copy(cagr5Year = None, volatility = None)))
        )
      )
      val cards = InvestmentScorer.rank(List(modified, candidate("B")), now, recipe)
      cards.forall(_.eligible) mustBe true
      cards.find(_.ticker == Ticker("A")).get.warnings must contain("insufficient-comparable-price-risk-history")
      cards.head.warnings must contain("reporting-currency-unknown-no-cross-currency-yields-computed")
    }

    "not exclude a company because older debt observations are missing" in {
      val input    = candidate("A")
      val modified = input.copy(financialSnapshot =
        input.financialSnapshot.map(s =>
          s.copy(
            annual = s.annual.filterNot(p => p.metric == FinancialMetric.DebtToAssets && p.period.getYear < 2025)
          )
        )
      )
      val cards = InvestmentScorer.rank(List(modified, candidate("B")), now, recipe)
      cards.forall(_.eligible) mustBe true
      cards.head.featureUnits(ScoringFeature.EarningsYield) mustBe "ratio"
    }

    List("Banks" -> Sector.Financials, "REIT" -> Sector.RealEstate).foreach { case (industry, sector) =>
      s"keep $industry companies and inactive securities out of industrial cohorts" in {
        val input    = candidate("SPECIALIST", industry = industry, sector = sector)
        val modified = input.copy(stock = input.stock.copy(security = input.stock.security.copy(isActive = false)))
        val card     = InvestmentScorer.rank(List(modified), now, recipe).head
        card.exclusions must contain allOf ("specialist-sector-model-required", "inactive-security")
      }
    }

    "not reward an attractive historical chart independently of fundamentals" in {
      val inputs  = List(candidate("A"), candidate("B"))
      val changed = inputs.map(c =>
        c.copy(stock =
          c.stock.copy(priceAnalytics = c.stock.priceAnalytics.map(p => p.copy(scores = p.scores.copy(overallScore = BigDecimal(100)))))
        )
      )
      InvestmentScorer.rank(changed, now, recipe) mustBe InvestmentScorer.rank(inputs, now, recipe)
    }

    "separately enforce price and quarterly fundamental freshness" in {
      val input    = candidate("A")
      val modified = input.copy(
        stock = input.stock.copy(priceAnalytics =
          input.stock.priceAnalytics.map(p =>
            p.copy(performanceSummary = p.performanceSummary.copy(latestPriceDate = LocalDate.of(2026, 1, 7)))
          )
        ),
        financialSnapshot =
          input.financialSnapshot.map(s => s.copy(quarterly = s.quarterly.map(_.copy(period = LocalDate.of(2025, 3, 31)))))
      )
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain allOf ("missing-invalid-or-stale-price", "invalid-or-stale-current-free-cash-flow")
    }

    "retain valid features and every independent reason when several financial inputs fail validation" in {
      val input    = candidate("INVALID")
      val modified = input.copy(financialSnapshot =
        input.financialSnapshot.map(snapshot =>
          snapshot.copy(
            observedAt = now.minusSeconds(15L * 86400),
            mappingVersion = "unverified",
            issues = List(FinancialSnapshotIssue.SymbolMismatch),
            metrics = (snapshot.metrics - FinancialMetric.PriceToEarningsTtm - FinancialMetric.RevenueGrowth5Y)
              .updated(FinancialMetric.PriceToFreeCashFlowTtm, BigDecimal(-1))
              .updated(FinancialMetric.EpsTtm, BigDecimal(0)),
            annual = snapshot.annual.filterNot(_.metric == FinancialMetric.ReturnOnAssets),
            quarterly = Nil
          )
        )
      )
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions mustBe List(
        "invalid-provider-observations",
        "stale-financial-snapshot",
        "insufficient-annual-history:return-on-assets",
        "missing-metric:price-to-earnings-ttm",
        "nonpositive-metric:price-to-free-cash-flow-ttm",
        "nonpositive-metric:eps-ttm",
        "missing-metric:revenue-growth5y",
        "missing-current-free-cash-flow"
      )
      card.features mustBe Map(
        ScoringFeature.EpsGrowth5Y                -> BigDecimal("0.15"),
        ScoringFeature.DebtToAssets               -> BigDecimal("0.6"),
        ScoringFeature.FreeCashFlowMargin5YMedian -> BigDecimal("0.1")
      )
      card.score mustBe None
    }

    "use the same cash-flow durability thresholds in eligibility and near-miss explanations" in {
      val input = candidate("CASH")
      val cases = List(
        (Set(2024), true, "4/5 positive years"),
        (Set(2025), false, "4/5 positive years"),
        (Set(2024, 2023), false, "3/5 positive years")
      )
      cases.foreach { case (negativeYears, eligible, count) =>
        val snapshot = input.financialSnapshot.get.copy(annual = input.financialSnapshot.get.annual.map { observation =>
          if observation.metric == FinancialMetric.FreeCashFlowMargin && negativeYears(observation.period.getYear) then
            observation.copy(value = BigDecimal("-0.1"))
          else observation
        })
        val modified = input.copy(financialSnapshot = Some(snapshot))
        val scored   = InvestmentScorer
          .rank(List(candidate("A"), candidate("B"), modified), now, recipe)
          .find(_.ticker == input.stock.security.ticker)
          .get

        scored.eligible mustBe eligible
        if eligible then Eligibility.explainExclusion(scored, Some(snapshot)) mustBe None
        else
          scored.exclusions mustBe List("insufficient-cash-flow-durability")
          val compact     = snapshot.copy(annual = Nil, quarterly = Nil)
          val explanation = Eligibility.explainExclusion(scored, Some(compact)).get
          explanation must include(count)
          explanation must include("requires latest > 0 and at least 4/5 positive years")
      }
    }
  }
