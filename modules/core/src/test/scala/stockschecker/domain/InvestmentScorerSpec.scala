package stockschecker.domain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import stockschecker.fixtures.*

import java.time.{Instant, LocalDate}

class InvestmentScorerSpec extends AnyWordSpec with Matchers:
  private val now = Instant.parse("2026-01-15T12:00:00Z")
  private val recipe = InvestmentRecipe(minIndustryPeers = 2, minSectorPeers = 3)

  private def candidate(symbol: String, strength: Int = 1, industry: String = "Technology"): InvestmentCandidate =
    val ticker = Ticker(symbol)
    val observations = (0 until 6).toList.flatMap { i =>
      val period = LocalDate.of(2025 - i, 9, 30)
      List(
        FinancialObservation("roa", period, BigDecimal("0.1") * strength, "ratio"),
        FinancialObservation("fcfMargin", period, BigDecimal("0.1") * strength, "ratio"),
        FinancialObservation("totalDebtToTotalAsset", period, BigDecimal("0.6") / strength, "ratio"),
        FinancialObservation("eps", period, BigDecimal(10 - i), "reporting-currency-per-share"),
        FinancialObservation("salesPerShare", period, BigDecimal(100 - 10 * i), "reporting-currency-per-share")
      )
    }
    val snapshot = FinancialSnapshot(symbol, ticker, "Finnhub", now,
      Map("peTTM" -> (BigDecimal(20) / strength), "pfcfShareTTM" -> (BigDecimal(30) / strength), "epsTTM" -> BigDecimal(10),
        "revenueGrowth5Y" -> BigDecimal(10 * strength), "epsGrowth5Y" -> BigDecimal(15 * strength)),
      observations, List(FinancialObservation("fcfPerShareTTM", LocalDate.of(2025, 9, 30), BigDecimal(5), "reporting-currency-per-share")),
      "{}", "hash", InvestmentScorer.MappingVersion, Nil)
    val analytics = AAPLPriceAnalytics.copy(ticker = ticker,
      performanceSummary = AAPLPriceAnalytics.performanceSummary.copy(latestPriceDate = LocalDate.of(2026, 1, 15)))
    InvestmentCandidate(Stock(AAPLSecurity.copy(ticker = ticker), Some(AAPLCompanyProfile.copy(ticker = ticker, industry = industry)), Some(analytics)), Some(snapshot))

  "InvestmentScorer" should {
    "rank stronger financial fundamentals with separately explained pillars" in {
      val cards = InvestmentScorer.rank(List(candidate("LOW"), candidate("HIGH", 2)), now, recipe)
      cards.map(_.ticker.value) mustBe List("HIGH", "LOW")
      cards.head.score mustBe Some(BigDecimal(100))
      cards.last.score mustBe Some(BigDecimal(0))
      cards.head.pillars.keySet mustBe Set("quality", "value", "growth", "resilience")
      cards.head.features("revenueGrowth5Y") mustBe BigDecimal("0.2")
      cards.head.warnings must contain("publication-time-unknown")
      cards.head.contributions mustBe Map("quality" -> BigDecimal(35), "value" -> BigDecimal(30), "growth" -> BigDecimal(20), "resilience" -> BigDecimal(15))
      cards.head.strengths must have size 4
      cards.last.weaknesses must have size 4
      val evidence = cards.head.evidence.get
      evidence.provider mustBe "Finnhub"
      evidence.observedAt mustBe now
      evidence.publishedAt mustBe None
      evidence.annualPeriods("roa") mustBe (2025 to 2021 by -1).toList.map(year => LocalDate.of(year, 9, 30))
      evidence.financialObservations.count(_.metric == "totalDebtToTotalAsset") mustBe 5
      evidence.latestFreeCashFlowPerShareTtm.map(_.value) mustBe Some(BigDecimal(5))
      evidence.reportingCurrency mustBe None
      cards.head.featureUnits("epsTtm") mustBe "reporting-currency-per-share"
    }

    "give tied observations midrank and order tied tickers deterministically" in {
      val inputs = List(candidate("ZZZ"), candidate("AAA"))
      val cards = InvestmentScorer.rank(inputs, now, recipe)
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
      val inputs = List(candidate("A", industry = "Software"), candidate("B", industry = "Hardware"), candidate("C", industry = "Semiconductors"))
      val cards = InvestmentScorer.rank(inputs, now, recipe)
      cards.forall(_.peerGroup.contains("sector:technology")) mustBe true
      cards.forall(_.peerCount == 3) mustBe true
    }

    "exclude missing value inputs instead of redistributing their weights" in {
      val incomplete = candidate("MISSING", 2)
      val modified = incomplete.copy(financialSnapshot = incomplete.financialSnapshot.map(s => s.copy(metrics = s.metrics - "pfcfShareTTM")))
      val cards = InvestmentScorer.rank(List(candidate("A"), candidate("B"), modified), now, recipe)
      val excluded = cards.find(_.ticker == Ticker("MISSING")).get
      excluded.score mustBe None
      excluded.exclusions must contain("missing-metric:pfcfShareTTM")
      cards.filter(_.eligible).forall(_.peerCount == 2) mustBe true
    }

    "exclude unavailable reported revenue growth rather than substituting sales per share" in {
      val input = candidate("A")
      val modified = input.copy(financialSnapshot = input.financialSnapshot.map(s => s.copy(metrics = s.metrics - "revenueGrowth5Y")))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.features must contain key "salesPerShare5YCagr"
      card.exclusions must contain("missing-metric:revenueGrowth5Y")
      card.score mustBe None
    }

    "reject stale fetches, invalid mappings and incomplete fiscal histories" in {
      val input = candidate("A")
      val modified = input.copy(financialSnapshot = input.financialSnapshot.map(s => s.copy(
        observedAt = now.minusSeconds(15L * 86400), mappingVersion = "unknown",
        annual = s.annual.filterNot(p => p.metric == "roa" && p.period.getYear == 2023)
      )))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain allOf("stale-financial-snapshot", "unverified-financial-mapping", "insufficient-annual-history:roa")
      card.score mustBe None
    }

    "exclude current losses without inventing a CAGR from negative optional history" in {
      val input = candidate("A")
      val modified = input.copy(financialSnapshot = input.financialSnapshot.map(s => s.copy(
        metrics = s.metrics.updated("epsTTM", BigDecimal(-1)),
        annual = s.annual.map(p => if p.metric == "eps" && p.period.getYear == 2020 then p.copy(value = BigDecimal(-1)) else p)
      )))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain("nonpositive-metric:epsTTM")
      card.features must not contain key("epsHistory5YCagr")
    }

    "require quality histories to cover the same fiscal periods" in {
      val input = candidate("A")
      val modified = input.copy(financialSnapshot = input.financialSnapshot.map(s => s.copy(
        annual = s.annual.map(p => if p.metric == "fcfMargin" then p.copy(period = p.period.minusYears(1)) else p)
      )))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain("misaligned-quality-fiscal-periods")
      card.exclusions must not contain "insufficient-annual-history:fcfMargin"
      card.score mustBe None
    }

    "reject incompatible required metric units before calculating ranks" in {
      val input = candidate("A")
      val modified = input.copy(financialSnapshot = input.financialSnapshot.map(s => s.copy(
        annual = s.annual.map(p => if p.metric == "roa" then p.copy(unit = "percent") else p),
        quarterly = s.quarterly.map(_.copy(unit = "provider-defined"))
      )))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain allOf("unverified-unit:roa", "unverified-unit:fcfPerShareTTM")
      card.features must not contain key("returnOnAssets5YMedian")
    }

    "warn separately about missing price risk history without changing financial eligibility" in {
      val input = candidate("A")
      val modified = input.copy(stock = input.stock.copy(priceAnalytics = input.stock.priceAnalytics.map(p =>
        p.copy(metrics = p.metrics.copy(cagr5Year = None, volatility = None)))))
      val cards = InvestmentScorer.rank(List(modified, candidate("B")), now, recipe)
      cards.forall(_.eligible) mustBe true
      cards.find(_.ticker == Ticker("A")).get.warnings must contain("insufficient-comparable-price-risk-history")
      cards.head.warnings must contain("reporting-currency-unknown-no-cross-currency-yields-computed")
    }

    "not exclude a company because optional EPS and sales history or older debt observations are missing" in {
      val input = candidate("A")
      val modified = input.copy(financialSnapshot = input.financialSnapshot.map(s => s.copy(
        annual = s.annual.filterNot(p => Set("salesPerShare", "eps")(p.metric) || (p.metric == "totalDebtToTotalAsset" && p.period.getYear < 2025))
      )))
      val cards = InvestmentScorer.rank(List(modified, candidate("B")), now, recipe)
      cards.forall(_.eligible) mustBe true
      cards.head.featureUnits("earningsYield") mustBe "ratio"
    }

    "keep financial-sector companies and inactive securities out of industrial cohorts" in {
      val input = candidate("BANK", industry = "Banks")
      val modified = input.copy(stock = input.stock.copy(security = input.stock.security.copy(isActive = false)))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain allOf("specialist-sector-model-required", "inactive-security")
    }

    "not reward an attractive historical chart independently of fundamentals" in {
      val inputs = List(candidate("A"), candidate("B"))
      val changed = inputs.map(c => c.copy(stock = c.stock.copy(priceAnalytics = c.stock.priceAnalytics.map(p =>
        p.copy(scores = p.scores.copy(overallScore = BigDecimal(100)))))))
      InvestmentScorer.rank(changed, now, recipe) mustBe InvestmentScorer.rank(inputs, now, recipe)
    }

    "separately enforce price and quarterly fundamental freshness" in {
      val input = candidate("A")
      val modified = input.copy(stock = input.stock.copy(priceAnalytics = input.stock.priceAnalytics.map(p =>
        p.copy(performanceSummary = p.performanceSummary.copy(latestPriceDate = LocalDate.of(2026, 1, 7))))),
        financialSnapshot = input.financialSnapshot.map(s => s.copy(quarterly = s.quarterly.map(_.copy(period = LocalDate.of(2025, 3, 31))))))
      val card = InvestmentScorer.rank(List(modified), now, recipe).head
      card.exclusions must contain allOf("missing-invalid-or-stale-price", "invalid-or-stale-current-free-cash-flow")
    }
  }
